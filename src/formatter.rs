use pulldown_cmark::MetadataBlockKind;

use super::*;

mod format;

impl<E> MarkdownFormatter<E>
where
    E: ExternalFormatter,
{
    /// Format Markdown input
    ///
    /// ```rust
    /// # use fmtm_ytmimi_markdown_fmt::MarkdownFormatter;
    /// let formatter = MarkdownFormatter::default();
    /// let input = "   #  Header! ";
    /// let rewrite = formatter.format(input).unwrap();
    /// assert_eq!(rewrite, String::from("# Header!"));
    /// ```
    pub fn format(self, input: &str) -> Result<String, std::fmt::Error> {
        // callback that will always revcover broken links
        let mut callback = |broken_link| {
            tracing::trace!("found boken link: {broken_link:?}");
            Some(("".into(), "".into()))
        };

        let mut options = Options::all();
        options.remove(Options::ENABLE_SMART_PUNCTUATION);

        let parser = Parser::new_with_broken_link_callback(input, options, Some(&mut callback));

        // There can't be any characters besides spaces, tabs, or newlines after the title
        // See https://spec.commonmark.org/0.30/#link-reference-definition for the
        // definition and https://spec.commonmark.org/0.30/#example-209 as an example.
        //
        // It seems that `pulldown_cmark` sometimes parses titles when it shouldn't.
        // To work around edge cases where a paragraph starting with a quoted string might be
        // interpreted as a link title we check that only whitespace follows the title
        let is_false_title = |input: &str, span: Range<usize>| {
            input[span.end..]
                .chars()
                .take_while(|c| *c != '\n')
                .any(|c| !c.is_whitespace())
        };

        let reference_links = parser
            .reference_definitions()
            .iter()
            .sorted_by(|(_, link_a), (_, link_b)| {
                // We want to sort these in descending order based on the ranges
                // This creates a stack of reference links that we can pop off of.
                link_b.span.start.cmp(&link_a.span.start)
            })
            // TODO: Fix typo.
            .map(|(link_lable, link_def)| {
                let (dest, title, span) = (&link_def.dest, &link_def.title, &link_def.span);
                let full_link = &input[span.clone()];
                if title.is_some() && is_false_title(input, span.clone()) {
                    let end = input[span.clone()]
                        .find(dest.as_ref())
                        .map(|idx| idx + dest.len())
                        .unwrap_or(span.end);
                    return (
                        link_lable.to_string(),
                        dest.to_string(),
                        None,
                        span.start..end,
                    );
                }

                if let Some((url, title)) = links::recover_escaped_link_destination_and_title(
                    full_link,
                    link_lable,
                    title.is_some(),
                ) {
                    (link_lable.to_string(), url, title, span.clone())
                } else {
                    // Couldn't recover URL from source, just use what we've been given
                    (
                        link_lable.to_string(),
                        dest.to_string(),
                        title.clone().map(|s| (s.to_string(), '"')),
                        span.clone(),
                    )
                }
            })
            .collect::<Vec<_>>();

        let iter = parser
            .into_offset_iter()
            .all_loose_lists()
            .all_sequential_blocks();

        let fmt_state = <FormatState<E, _>>::new(input, self.config, iter, reference_links);
        fmt_state.format()
    }
}

type ReferenceLinkDefinition = (String, String, Option<(String, char)>, Range<usize>);

pub(crate) struct FormatState<'i, E, I>
where
    E: ExternalFormatter,
    I: Iterator<Item = (Event<'i>, std::ops::Range<usize>)>,
{
    /// Raw markdown input
    input: &'i str,
    pub(crate) last_was_softbreak: bool,
    /// Iterator Supplying Markdown Events
    events: Peekable<I>,
    rewrite_buffer: String,
    /// Handles code block, HTML block, and paragraph formatting.
    external_formatter: Option<E>,
    /// Stack that keeps track of nested list markers.
    /// Unordered list markers are one of `*`, `+`, or `-`,
    /// while ordered lists markers start with 0-9 digits followed by a `.` or `)`.
    // TODO(ytmimi) Add a configuration to allow incrementing ordered lists
    // list_markers: Vec<ListMarker>,
    /// Stack that keeps track of indentation.
    indentation: Vec<Cow<'static, str>>,
    /// Stack that keeps track of whether we're formatting inside of another element.
    nested_context: Vec<Tag<'i>>,
    /// A set of reference link definitions that will be output after formatting.
    /// Reference style links contain 3 parts:
    /// 1. Text to display
    /// 2. URL
    /// 3. (Optional) Title
    /// ```markdown
    /// [title]: link "optional title"
    /// ```
    reference_links: Vec<ReferenceLinkDefinition>,
    /// keep track of the current setext header.
    /// ```markdown
    /// Header
    /// ======
    /// ```
    setext_header: Option<&'i str>,
    /// Store the fragment identifier and classes from the header start tag.
    header_id_and_classes: Option<(Option<CowStr<'i>>, Vec<CowStr<'i>>)>,
    /// next Start event should push indentation
    needs_indent: bool,
    table_state: Option<TableState<'i>>,
    last_position: usize,
    trim_link_or_image_start: bool,
    /// Force write into rewrite buffer.
    // TODO: Remove this after making an adapter to solve the stupid
    // out-of-order problem.
    force_rewrite_buffer: bool,
    /// Format configurations
    config: Config,
}

/// Depnding on the formatting context there are a few different buffers where we might want to
/// write formatted markdown events. The Write impl helps us centralize this logic.
impl<'i, E, I> Write for FormatState<'i, E, I>
where
    I: Iterator<Item = (Event<'i>, std::ops::Range<usize>)>,
    E: ExternalFormatter,
{
    fn write_str(&mut self, text: &str) -> std::fmt::Result {
        if let Some(writer) = self.current_buffer() {
            tracing::trace!(text, "write_str");
            writer.write_str(text)?
        }
        Ok(())
    }

    fn write_fmt(&mut self, args: std::fmt::Arguments<'_>) -> std::fmt::Result {
        if let Some(writer) = self.current_buffer() {
            writer.write_fmt(args)?
        }
        Ok(())
    }
}

impl<'i, E, I> FormatState<'i, E, I>
where
    E: ExternalFormatter,
    I: Iterator<Item = (Event<'i>, std::ops::Range<usize>)>,
{
    /// Peek at the next Markdown Event
    fn peek(&mut self) -> Option<&Event<'i>> {
        self.events.peek().map(|(e, _)| e)
    }

    /// Peek at the next Markdown Event and it's original position in the input
    fn peek_with_range(&mut self) -> Option<(&Event, &Range<usize>)> {
        self.events.peek().map(|(e, r)| (e, r))
    }

    /// Check if the next Event is an `Event::End`
    fn is_next_end_event(&mut self) -> bool {
        matches!(self.peek(), Some(Event::End(_)))
    }

    /// Check if we should write newlines and indentation before the next Start Event
    fn check_needs_indent(&mut self, event: &Event<'i>) {
        self.needs_indent = match self.peek() {
            Some(Event::Start(_) | Event::Rule | Event::Html(_) | Event::End(TagEnd::Item)) => true,
            Some(Event::End(TagEnd::BlockQuote)) => matches!(event, Event::End(_)),
            Some(Event::Text(_)) => matches!(event, Event::End(_) | Event::Start(Tag::Item)),
            _ => matches!(event, Event::Rule),
        };
    }

    /// Check if we're formatting a fenced code block
    fn in_fenced_code_block(&self) -> bool {
        matches!(
            self.nested_context.last(),
            Some(Tag::CodeBlock(CodeBlockKind::Fenced(_)))
        )
    }

    /// Check if we're formatting an indented code block
    fn in_indented_code_block(&self) -> bool {
        matches!(
            self.nested_context.last(),
            Some(Tag::CodeBlock(CodeBlockKind::Indented))
        )
    }

    /// Check if we're in an HTML block.
    fn in_html_block(&self) -> bool {
        Some(FormattingContext::HtmlBlock) == self.external_formatter.as_ref().map(|f| f.context())
    }

    // check if we're formatting a table header
    fn in_table_header(&self) -> bool {
        self.nested_context
            .iter()
            .rfind(|tag| **tag == Tag::TableHead)
            .is_some()
    }

    // check if we're formatting a table row
    fn in_table_row(&self) -> bool {
        self.nested_context
            .iter()
            .rfind(|tag| **tag == Tag::TableRow)
            .is_some()
    }

    /// Check if we're formatting a link
    fn in_link_or_image(&self) -> bool {
        matches!(
            self.nested_context.last(),
            Some(Tag::Link { .. } | Tag::Image { .. })
        )
    }

    /// Check if we're in a "paragraph". A `Paragraph` might not necessarily be on the
    /// nested_context stack.
    fn in_paragraph(&self) -> bool {
        Some(FormattingContext::Paragraph) == self.external_formatter.as_ref().map(|f| f.context())
    }

    /// Check if we're formatting in a nested context
    fn is_nested(&self) -> bool {
        !self.nested_context.is_empty()
    }

    /// Get the length of the indentation
    fn indentation_len(&self) -> usize {
        self.indentation.iter().map(|i| i.len()).sum()
    }

    /// Get an exclusive reference to the current buffer we're writing to. That could be the main
    /// rewrite buffer, the code block buffer, the internal table state, or anything else we're
    /// writing to while reformatting
    fn current_buffer(&mut self) -> Option<&mut dyn std::fmt::Write> {
        if self.force_rewrite_buffer {
            tracing::trace!("rewrite_buffer");
            Some(&mut self.rewrite_buffer)
        } else if self.in_fenced_code_block() || self.in_indented_code_block() {
            tracing::trace!("code_block_buffer");
            self.external_formatter
                .as_mut()
                .map(|f| f as &mut dyn std::fmt::Write)
        } else if self.in_html_block() {
            tracing::trace!("html_block");
            self.external_formatter
                .as_mut()
                .map(|h| h as &mut dyn std::fmt::Write)
        } else if self.in_table_header() || self.in_table_row() {
            tracing::trace!("table_state");
            self.table_state
                .as_mut()
                .map(|s| s as &mut dyn std::fmt::Write)
        } else if self.in_paragraph() {
            tracing::trace!("paragraph");
            self.external_formatter
                .as_mut()
                .map(|p| p as &mut dyn std::fmt::Write)
        } else {
            tracing::trace!("rewrite_buffer");
            Some(&mut self.rewrite_buffer)
        }
    }

    /// Check if the current buffer we're writting to is empty
    fn is_current_buffer_empty(&self) -> bool {
        if self.in_fenced_code_block() || self.in_indented_code_block() || self.in_html_block() {
            self.external_formatter
                .as_ref()
                .is_some_and(ExternalFormatter::is_empty)
        } else if self.in_table_header() || self.in_table_row() {
            self.table_state.as_ref().is_some_and(|s| s.is_empty())
        } else if self.in_paragraph() {
            self.external_formatter
                .as_ref()
                .is_some_and(ExternalFormatter::is_empty)
        } else {
            self.rewrite_buffer.is_empty()
        }
    }

    fn count_newlines(&self, range: &Range<usize>) -> usize {
        if self.last_position == range.start {
            return 0;
        }

        let snippet = if self.last_position < range.start {
            // between two markdown evernts
            &self.input[self.last_position..range.start]
        } else {
            // likely in some nested context
            self.input[self.last_position..range.end].trim_end_matches('\n')
        };

        snippet.chars().filter(|char| *char == '\n').count()
    }

    fn write_indentation(&mut self, trim_trailing_whiltespace: bool) -> std::fmt::Result {
        let last_non_complete_whitespace_indent = self
            .indentation
            .iter()
            .rposition(|indent| !indent.chars().all(char::is_whitespace));

        let position = if trim_trailing_whiltespace {
            let Some(position) = last_non_complete_whitespace_indent else {
                // All indents are just whitespace. We don't want to push blank lines
                return Ok(());
            };
            position
        } else {
            self.indentation.len()
        };

        // Temporarily take indentation to work around the borrow checker
        let indentation = std::mem::take(&mut self.indentation);

        for (i, indent) in indentation.iter().take(position + 1).enumerate() {
            let is_last = i == position;

            if is_last && trim_trailing_whiltespace {
                self.write_str(indent.trim())?;
            } else {
                self.write_str(indent)?;
            }
        }
        // Put the indentation back!
        self.indentation = indentation;
        Ok(())
    }

    fn write_newlines(&mut self, max_newlines: usize) -> std::fmt::Result {
        self.write_newlines_inner(max_newlines, false)
    }

    fn write_newlines_no_trailing_whitespace(&mut self, max_newlines: usize) -> std::fmt::Result {
        self.write_newlines_inner(max_newlines, true)
    }

    fn write_newlines_inner(
        &mut self,
        max_newlines: usize,
        always_trim_trailing_whitespace: bool,
    ) -> std::fmt::Result {
        if self.is_current_buffer_empty() {
            return Ok(());
        }
        let newlines = self
            .rewrite_buffer
            .chars()
            .rev()
            .take_while(|c| *c == '\n')
            .count();

        let nested = self.is_nested();
        let newlines_to_write = max_newlines.saturating_sub(newlines);
        let next_is_end_event = self.is_next_end_event();
        tracing::trace!(newlines, nested, newlines_to_write, next_is_end_event);

        for i in 0..newlines_to_write {
            let is_last = i == newlines_to_write - 1;

            writeln!(self)?;

            if nested {
                self.write_indentation(!is_last || always_trim_trailing_whitespace)?;
            }
        }
        if !nested {
            self.write_indentation(next_is_end_event || always_trim_trailing_whitespace)?;
        }
        Ok(())
    }

    fn write_indentation_if_needed(&mut self) -> Result<bool, std::fmt::Error> {
        match self.rewrite_buffer.chars().last() {
            Some('\n') | None => {
                self.write_indentation(false)?;
                Ok(true)
            }
            _ => Ok(false),
        }
    }

    fn write_reference_link_definition_inner(
        &mut self,
        label: &str,
        dest: &str,
        title: Option<&(String, char)>,
    ) -> std::fmt::Result {
        // empty links can be specified with <>
        let dest = links::format_link_url(dest, true);
        self.write_newlines(1)?;
        if let Some((title, quote)) = title {
            write!(self, r#"[{}]: {dest} {quote}{title}{quote}"#, label.trim())?;
        } else {
            write!(self, "[{}]: {dest}", label.trim())?;
        }
        Ok(())
    }

    fn rewrite_reference_link_definitions(&mut self, range: &Range<usize>) -> std::fmt::Result {
        if self.reference_links.is_empty() {
            return Ok(());
        }
        // use std::mem::take to work around the borrow checker
        let mut reference_links = std::mem::take(&mut self.reference_links);

        loop {
            match reference_links.last() {
                Some((_, _, _, link_range)) if link_range.start > range.start => {
                    // The reference link on the top of the stack comes further along in the file
                    break;
                }
                None => break,
                _ => {}
            }

            let (label, dest, title, link_range) = reference_links.pop().expect("we have a value");
            let newlines = self.count_newlines(&link_range);
            self.write_newlines(newlines)?;
            self.write_reference_link_definition_inner(&label, &dest, title.as_ref())?;
            self.last_position = link_range.end;
            self.needs_indent = true;
        }

        // put the reference_links back
        self.reference_links = reference_links;
        Ok(())
    }

    /// Write out reference links at the end of the file
    fn rewrite_final_reference_links(mut self) -> Result<String, std::fmt::Error> {
        // use std::mem::take to work around the borrow checker
        let reference_links = std::mem::take(&mut self.reference_links);

        // need to iterate in reverse because reference_links is a stack
        for (label, dest, title, range) in reference_links.into_iter().rev() {
            let newlines = self.count_newlines(&range);
            self.write_newlines(newlines)?;

            // empty links can be specified with <>
            self.write_reference_link_definition_inner(&label, &dest, title.as_ref())?;
            self.last_position = range.end
        }
        Ok(self.rewrite_buffer)
    }

    fn join_with_indentation(
        &mut self,
        buffer: &str,
        start_with_indentation: bool,
    ) -> std::fmt::Result {
        self.force_rewrite_buffer = true;
        let mut lines = buffer.trim_end().lines().peekable();
        while let Some(line) = lines.next() {
            let is_last = lines.peek().is_none();
            let is_next_empty = lines
                .peek()
                .map(|l| l.trim().is_empty())
                .unwrap_or_default();

            if start_with_indentation {
                self.write_indentation(line.trim().is_empty())?;
            }

            if !line.trim().is_empty() {
                self.write_str(line)?;
            }

            if !is_last {
                writeln!(self)?;
            }

            if !is_last && !start_with_indentation {
                self.write_indentation(is_next_empty)?;
            }
        }
        self.force_rewrite_buffer = false;
        Ok(())
    }
}

impl<'i, E, I> FormatState<'i, E, I>
where
    I: Iterator<Item = (Event<'i>, std::ops::Range<usize>)>,
    E: ExternalFormatter,
{
    pub(crate) fn new(
        input: &'i str,
        config: Config,
        iter: I,
        reference_links: Vec<ReferenceLinkDefinition>,
    ) -> Self {
        Self {
            input,
            last_was_softbreak: false,
            events: iter.peekable(),
            rewrite_buffer: String::with_capacity(input.len() * 2),
            external_formatter: None,
            // TODO(ytmimi) Add a configuration to allow incrementing ordered lists
            // list_markers: vec![],
            indentation: vec![],
            nested_context: vec![],
            reference_links,
            setext_header: None,
            header_id_and_classes: None,
            needs_indent: false,
            table_state: None,
            last_position: 0,
            trim_link_or_image_start: false,
            force_rewrite_buffer: false,
            config,
        }
    }

    /* fn write_code_block_buffer(&mut self, info: Option<&str>) -> std::fmt::Result {
        let code_block = E::new(BufferType::CodeBlock { info }, self.config.max_width, );
        let code = self.format_code_buffer(info_string);

        if code.trim().is_empty() && info_string.is_some() {
            // The code fence is empty, and a newline should already ahve been added
            // when pushing the opening code fence, so just return.
            return Ok(());
        }

        self.join_with_indentation(&code, info_string.is_some())?;

        if info_string.is_some() {
            // In preparation for the closing code fence write a newline.
            writeln!(self)?
        }

        Ok(())
    } */

    fn formatter_width(&self) -> Option<usize> {
        self.config
            .max_width
            .map(|w| w.saturating_sub(self.indentation_len()))
    }

    /// The main entry point for markdown formatting.
    pub fn format(mut self) -> Result<String, std::fmt::Error> {
        while let Some((event, range)) = self.events.next() {
            let mut last_position = self.input[..range.end]
                .char_indices()
                .rev()
                .find(|(_, char)| !char.is_whitespace())
                .map(|(index, _)| index)
                .unwrap_or(0);
            tracing::debug!(?event, ?range, last_position);

            match event {
                Event::Start(tag) => {
                    self.rewrite_reference_link_definitions(&range)?;
                    last_position = range.start;
                    self.start_tag(tag.clone(), range)?;
                }
                Event::End(ref tag) => {
                    self.end_tag(*tag, range)?;
                    self.check_needs_indent(&event);
                }
                // TODO: Format display math with its own buffer.
                Event::Text(ref parsed_text) | Event::DisplayMath(ref parsed_text) => {
                    if self
                        .external_formatter
                        .as_ref()
                        .is_some_and(|f| f.context() != FormattingContext::Paragraph)
                    {
                        // External formatting. Write the text as is.
                        self.write_str(parsed_text)?;
                    } else {
                        last_position = range.end;
                        let starts_with_escape = self.input[..range.start].ends_with('\\');
                        let newlines = self.count_newlines(&range);
                        let text_from_source = &self.input[range];
                        let mut text = if text_from_source.is_empty() {
                            // This seems to happen when the parsed text is whitespace only.
                            // To preserve leading whitespace outside of HTML blocks,
                            // use the parsed text instead.
                            parsed_text.as_ref()
                        } else {
                            text_from_source
                        };

                        if self.in_link_or_image() && self.trim_link_or_image_start {
                            // Trim leading whitespace from reference links or images
                            text = text.trim_start();
                            // Make sure we only trim leading whitespace once
                            self.trim_link_or_image_start = false
                        }

                        if matches!(
                            self.peek(),
                            Some(Event::End(TagEnd::Link { .. } | TagEnd::Image { .. }))
                        ) {
                            text = text.trim_end();
                        }

                        if self.needs_indent {
                            self.write_newlines(newlines)?;
                        }

                        if starts_with_escape || self.needs_escape(text) {
                            // recover escape characters
                            write!(self, "\\{text}")?;
                        } else {
                            write!(self, "{text}")?;
                        }
                        self.check_needs_indent(&event);
                    }
                }
                Event::Code(_) => {
                    write!(self, "{}", &self.input[range])?;
                }
                Event::SoftBreak => {
                    last_position = range.end;
                    if self.in_link_or_image() {
                        let next_is_end = matches!(
                            self.peek(),
                            Some(Event::End(TagEnd::Link { .. } | TagEnd::Image { .. }))
                        );
                        if self.trim_link_or_image_start || next_is_end {
                            self.trim_link_or_image_start = false
                        } else {
                            write!(self, " ")?;
                        }
                    } else {
                        write!(self, "{}", &self.input[range])?;

                        // paraphraphs write their indentation after reformatting the text
                        if !self.in_paragraph() {
                            self.write_indentation(false)?;
                        }

                        self.last_was_softbreak = true;
                    }
                }
                Event::HardBreak => {
                    write!(self, "{}", &self.input[range])?;
                }
                Event::Html(_) => {
                    // NOTE: This limitation is because Pulldown-CMark
                    // incorrectly include spaces before HTML.
                    let html = &self.input[range].trim_start_matches(' ');
                    write!(self, "{}", html)?; // Write HTML as is.
                    self.check_needs_indent(&event);
                }
                Event::InlineHtml(_) | Event::InlineMath(_) => {
                    let newlines = self.count_newlines(&range);
                    if self.needs_indent {
                        self.write_newlines(newlines)?;
                    }
                    self.write_str(self.input[range].trim_end_matches('\n'))?;
                    self.check_needs_indent(&event);
                }
                Event::Rule => {
                    let newlines = self.count_newlines(&range);
                    self.write_newlines(newlines)?;
                    write!(self, "{}", &self.input[range])?;
                    self.check_needs_indent(&event)
                }
                Event::FootnoteReference(text) => {
                    write!(self, "[^{text}]")?;
                }
                Event::TaskListMarker(done) => {
                    if done {
                        write!(self, "[x] ")?;
                    } else {
                        write!(self, "[ ] ")?;
                    }
                }
            }
            self.last_position = last_position
        }
        debug_assert!(self.nested_context.is_empty());
        let trailing_newline = self.input.ends_with('\n');
        self.rewrite_final_reference_links().map(|mut output| {
            if trailing_newline {
                output.push('\n');
            }
            output
        })
    }

    fn start_tag(&mut self, tag: Tag<'i>, range: Range<usize>) -> std::fmt::Result {
        match tag {
            Tag::Paragraph => {
                if self.needs_indent {
                    let newlines = self.count_newlines(&range);
                    self.write_newlines(newlines)?;
                    self.needs_indent = false;
                }
                self.nested_context.push(tag);
                let capacity = (range.end - range.start) * 2;
                self.new_external_formatted(BufferType::Paragraph, capacity)?;
            }
            Tag::Heading {
                level, id, classes, ..
            } => {
                self.header_id_and_classes = Some((id, classes));
                if self.needs_indent {
                    let newlines = self.count_newlines(&range);
                    self.write_newlines(newlines)?;
                    self.needs_indent = false;
                }
                let full_header = self.input[range].trim();

                if full_header.contains('\n') && full_header.ends_with(['=', '-']) {
                    // support for alternative syntax for H1 and H2
                    // <https://www.markdownguide.org/basic-syntax/#alternate-syntax>
                    let header_marker = full_header.split('\n').last().unwrap().trim();
                    self.setext_header.replace(header_marker);
                    // setext header are handled in `end_tag`
                    return Ok(());
                }

                let header = match level {
                    HeadingLevel::H1 => "# ",
                    HeadingLevel::H2 => "## ",
                    HeadingLevel::H3 => "### ",
                    HeadingLevel::H4 => "#### ",
                    HeadingLevel::H5 => "##### ",
                    HeadingLevel::H6 => "###### ",
                };

                let empty_header = full_header
                    .trim_start_matches(header)
                    .trim_end_matches(|c: char| c.is_whitespace() || matches!(c, '#' | '\\'))
                    .is_empty();

                if empty_header {
                    write!(self, "{}", header.trim())?;
                } else {
                    write!(self, "{header}")?;
                }
            }
            Tag::BlockQuote(_) => {
                // Just in case we're starting a new block quote in a nested context where
                // We alternate indentation levels we want to remove trailing whitespace
                // from the blockquote that we're about to push on top of
                if let Some(indent) = self.indentation.last_mut() {
                    if indent == "> " {
                        *indent = ">".into()
                    }
                }

                let newlines = self.count_newlines(&range);
                if self.needs_indent {
                    self.write_newlines(newlines)?;
                    self.needs_indent = false;
                }

                self.nested_context.push(tag);

                match self.peek_with_range().map(|(e, r)| (e.clone(), r.clone())) {
                    Some((Event::End(TagEnd::BlockQuote), _)) => {
                        // The next event is `End(BlockQuote)` so the current blockquote is empty!
                        write!(self, ">")?;
                        self.indentation.push(">".into());

                        let newlines = count_newlines(self.input[range].trim_end());
                        self.write_newlines(newlines)?;
                    }
                    Some((Event::Start(Tag::BlockQuote(_)), next_range)) => {
                        // The next event is `Start(BlockQuote) so we're adding another level
                        // of indentation.
                        write!(self, ">")?;
                        self.indentation.push(">".into());

                        // Now add any missing newlines for empty block quotes between
                        // the current start and the next start
                        let newlines = count_newlines(&self.input[range.start..next_range.start]);
                        self.write_newlines(newlines)?;
                    }
                    Some((_, next_range)) => {
                        // Now add any missing newlines for empty block quotes between
                        // the current start and the next start
                        let newlines = count_newlines(&self.input[range.start..next_range.start]);

                        self.indentation.push("> ".into());
                        if newlines > 0 {
                            write!(self, ">")?;
                            self.write_newlines(newlines)?;
                        } else {
                            write!(self, "> ")?;
                        }
                    }
                    None => {
                        // Peeking at the next event should always return `Some()` for start events
                        unreachable!("At the very least we'd expect an `End(BlockQuote)` event.");
                    }
                }
            }
            Tag::CodeBlock(ref kind) => {
                let newlines = self.count_newlines(&range);
                for _ in 0..newlines {
                    self.write_char('\n')?;
                }
                let info = match kind {
                    CodeBlockKind::Fenced(info_string) => {
                        self.write_indentation_if_needed()?;
                        rewrite_marker(self.input, &range, self)?;

                        if info_string.is_empty() {
                            writeln!(self)?;
                            None
                        } else {
                            let exclude_fence =
                                self.input[range.clone()].trim_start_matches(['`', '~']);
                            let starts_with_space = exclude_fence
                                .trim_start_matches(['`', '~'])
                                .starts_with(char::is_whitespace);

                            let info_string = exclude_fence
                                .lines()
                                .next()
                                .unwrap_or_else(|| info_string.as_ref())
                                .trim();

                            if starts_with_space {
                                writeln!(self, " {info_string}")?;
                            } else {
                                writeln!(self, "{info_string}")?;
                            }
                            /* if !matches!(self.peek(), Some(Event::End(TagEnd::CodeBlock))) {
                                // Only write indentation if this isn't an empty indented code block
                                self.write_indentation(false)?;
                            } */
                            Some(info_string)
                        }
                    }
                    CodeBlockKind::Indented => {
                        // TODO(ytmimi) support tab as an indent
                        let indentation = "    ";
                        self.indentation.push(indentation.into());
                        /* if !matches!(self.peek(), Some(Event::End(TagEnd::CodeBlock))) {
                            // Only write indentation if this isn't an empty indented code block
                            self.write_str(indentation)?;
                        } */
                        None
                    }
                };
                self.new_external_formatted(BufferType::CodeBlock { info }, range.len() * 2)?;
                self.nested_context.push(tag);
            }
            Tag::List(_) => {
                if self.needs_indent {
                    let newlines = self.count_newlines(&range);
                    self.write_newlines(newlines)?;
                    self.needs_indent = false;
                }

                // TODO(ytmimi) Add a configuration to allow incrementing ordered lists
                // let list_marker = ListMarker::from_str(&self.input[range])
                //    .expect("Should be able to parse a list marker");
                // self.list_markers.push(list_marker);
                self.nested_context.push(tag);
            }
            Tag::Item => {
                let newlines = self.count_newlines(&range);
                if self.needs_indent && newlines > 0 {
                    self.write_newlines(newlines)?;
                }

                let empty_list_item = match self.events.peek() {
                    Some((Event::End(TagEnd::Item), _)) => true,
                    Some((_, next_range)) => {
                        let snippet = &self.input[range.start..next_range.start];
                        // It's an empty list if there are newlines between the list marker
                        // and the next event. For example,
                        //
                        // ```markdown
                        // -
                        //   foo
                        // ```
                        snippet.chars().any(|char| char == '\n')
                    }
                    None => false,
                };

                // We need to push a newline and indentation before the next event if
                // this is an empty list item
                self.needs_indent = empty_list_item;

                let list_marker = self
                    .config
                    .list_marker(&self.input[range.clone()])
                    .expect("Should be able to parse a list marker");
                tracing::debug!(?list_marker, source = &self.input[range]);
                // TODO(ytmimi) Add a configuration to allow incrementing ordered lists
                // Take list_marker so we can use `write!(self, ...)`
                // let mut list_marker = self
                //     .list_markers
                //     .pop()
                //     .expect("can't have list item without marker");
                let marker_char = list_marker.marker_char();
                match &list_marker {
                    ListMarker::Ordered { number, .. } if empty_list_item => {
                        let zero_padding = list_marker.zero_padding();
                        write!(self, "{zero_padding}{number}{marker_char}")?;
                    }
                    ListMarker::Ordered { number, .. } => {
                        let zero_padding = list_marker.zero_padding();
                        write!(self, "{zero_padding}{number}{marker_char} ")?;
                    }
                    ListMarker::Unordered(_) if empty_list_item => {
                        write!(self, "{marker_char}")?;
                    }
                    ListMarker::Unordered(_) => {
                        write!(self, "{marker_char} ")?;
                    }
                }

                self.nested_context.push(tag);
                // Increment the list marker in case this is a ordered list and
                // swap the list marker we took earlier
                self.indentation.push(
                    self.config
                        .fixed_indentation
                        .clone()
                        .unwrap_or_else(|| list_marker.indentation()),
                );
                // TODO(ytmimi) Add a configuration to allow incrementing ordered lists
                // list_marker.increment_count();
                // self.list_markers.push(list_marker)
            }
            Tag::FootnoteDefinition(label) => {
                write!(self, "[^{label}]: ")?;
            }
            Tag::Emphasis => {
                self.write_emphasis_marker(&range)?;
            }
            Tag::Strong => {
                self.write_strong_marker(&range)?;
            }
            Tag::Strikethrough => {
                rewrite_marker(self.input, &range, self)?;
            }
            Tag::Link { link_type, .. } => {
                let newlines = self.count_newlines(&range);
                if self.needs_indent && newlines > 0 {
                    self.write_newlines(newlines)?;
                    self.needs_indent = false;
                }

                let email_or_auto = matches!(link_type, LinkType::Email | LinkType::Autolink);
                let opener = if email_or_auto { "<" } else { "[" };
                self.write_str(opener)?;
                self.nested_context.push(tag);

                if matches!(self.peek(), Some(Event::Text(_) | Event::SoftBreak)) {
                    self.trim_link_or_image_start = true
                }
            }
            Tag::Image { .. } => {
                let newlines = self.count_newlines(&range);
                if self.needs_indent && newlines > 0 {
                    self.write_newlines(newlines)?;
                    self.needs_indent = false;
                }

                write!(self, "![")?;
                self.nested_context.push(tag);

                if matches!(self.peek(), Some(Event::Text(_) | Event::SoftBreak)) {
                    self.trim_link_or_image_start = true
                }
            }
            Tag::Table(ref alignment) => {
                if self.needs_indent {
                    let newlines = self.count_newlines(&range);
                    self.write_newlines(newlines)?;
                    self.needs_indent = false;
                }
                self.table_state.replace(TableState::new(alignment.clone()));
                write!(self, "|")?;
                self.indentation.push("|".into());
                self.nested_context.push(tag);
            }
            Tag::TableHead => {
                self.nested_context.push(tag);
            }
            Tag::TableRow => {
                self.nested_context.push(tag);
                if let Some(state) = self.table_state.as_mut() {
                    state.push_row()
                }
            }
            Tag::TableCell => {
                if !matches!(self.peek(), Some(Event::End(TagEnd::TableCell))) {
                    return Ok(());
                }

                if let Some(state) = self.table_state.as_mut() {
                    state.write(String::new().into());
                }
            }
            Tag::HtmlBlock => {
                let newlines = self.count_newlines(&range);
                tracing::trace!(newlines);
                self.flush_external_formatted()?;
                for _ in 0..newlines {
                    self.write_char('\n')?;
                }

                self.new_external_formatted(BufferType::HtmlBlock, range.len() * 2)?;
            }
            Tag::MetadataBlock(kind) => {
                self.write_metadata_block_separator(&kind, range)?;
            }
        }
        Ok(())
    }

    fn end_tag(&mut self, tag: TagEnd, range: Range<usize>) -> std::fmt::Result {
        match tag {
            TagEnd::Paragraph => {
                let popped_tag = self.nested_context.pop();
                debug_assert_eq!(popped_tag, Some(Tag::Paragraph));
                self.flush_external_formatted()?;
            }
            TagEnd::Heading(_) => {
                let (fragment_identifier, classes) = self
                    .header_id_and_classes
                    .take()
                    .expect("Should have pushed a header tag");
                match (fragment_identifier, classes.is_empty()) {
                    (Some(id), false) => {
                        let classes = rewirte_header_classes(classes)?;
                        write!(self, " {{#{id}{classes}}}")?;
                    }
                    (Some(id), true) => {
                        write!(self, " {{#{id}}}")?;
                    }
                    (None, false) => {
                        let classes = rewirte_header_classes(classes)?;
                        write!(self, " {{{}}}", classes.trim())?;
                    }
                    (None, true) => {}
                }

                if let Some(marker) = self.setext_header.take() {
                    self.write_newlines(1)?;
                    write!(self, "{marker}")?;
                }
            }
            TagEnd::BlockQuote => {
                let newlines = self.count_newlines(&range);
                if self.needs_indent && newlines > 0 {
                    // Recover empty block quote lines
                    if let Some(last) = self.indentation.last_mut() {
                        // Avoid trailing whitespace by replacing the last indentation with '>'
                        *last = ">".into()
                    }
                    self.write_newlines(newlines)?;
                }
                let popped_tag = self.nested_context.pop();
                debug_assert_eq!(popped_tag.unwrap().to_end(), tag);

                let popped_indentation = self
                    .indentation
                    .pop()
                    .expect("we pushed a blockquote marker in start_tag");
                if let Some(indentation) = self.indentation.last_mut() {
                    if indentation == ">" {
                        *indentation = popped_indentation
                    }
                }
            }
            TagEnd::CodeBlock => {
                let empty_code_block = self
                    .external_formatter
                    .as_ref()
                    .is_some_and(|f| f.is_empty());
                self.flush_external_formatted()?;

                let popped_tag = self.nested_context.pop();
                let Some(Tag::CodeBlock(kind)) = &popped_tag else {
                    unreachable!("Should have pushed a code block start tag");
                };
                match kind {
                    CodeBlockKind::Fenced(_) => {
                        // write closing code fence
                        if !empty_code_block
                            && !matches!(self.rewrite_buffer.chars().last(), Some('\n'))
                        {
                            writeln!(self)?;
                        }
                        self.write_indentation(false)?;
                        rewrite_marker(self.input, &range, self)?;
                    }
                    CodeBlockKind::Indented => {
                        let popped_indentation = self
                            .indentation
                            .pop()
                            .expect("we added 4 spaces in start_tag");
                        debug_assert_eq!(popped_indentation, "    ");
                    }
                }
            }
            TagEnd::List(_) => {
                let popped_tag = self.nested_context.pop();
                debug_assert_eq!(popped_tag.unwrap().to_end(), tag);
                // TODO(ytmimi) Add a configuration to allow incrementing ordered lists
                // self.list_markers.pop();

                // To prevent the next code block from being interpreted as a list we'll add an
                // HTML comment See https://spec.commonmark.org/0.30/#example-308, which states:
                //
                //     To separate consecutive lists of the same type, or to separate a list from an
                //     indented code block that would otherwise be parsed as a subparagraph of the
                //     final list item, you can insert a blank HTML comment
                if let Some(Event::Start(Tag::CodeBlock(CodeBlockKind::Indented))) = self.peek() {
                    self.write_newlines(1)?;
                    writeln!(self, "<!-- Don't absorb code block into list -->")?;
                    write!(self, "<!-- Consider a fenced code block instead -->")?;
                };
            }
            TagEnd::Item => {
                let newlines = self.count_newlines(&range);
                if self.needs_indent && newlines > 0 {
                    self.write_newlines_no_trailing_whitespace(newlines)?;
                }
                let popped_tag = self.nested_context.pop();
                debug_assert_eq!(popped_tag.unwrap().to_end(), tag);
                let popped_indentation = self.indentation.pop();
                debug_assert!(popped_indentation.is_some());

                // if the next event is a Start(Item), then we need to set needs_indent
                self.needs_indent = matches!(self.peek(), Some(Event::Start(Tag::Item)));
            }
            TagEnd::FootnoteDefinition => {}
            TagEnd::Emphasis => {
                self.write_emphasis_marker(&range)?;
            }
            TagEnd::Strong => {
                self.write_strong_marker(&range)?;
            }
            TagEnd::Strikethrough => {
                rewrite_marker(self.input, &range, self)?;
            }
            TagEnd::Link | TagEnd::Image => {
                let popped_tag = self
                    .nested_context
                    .pop()
                    .expect("Should have pushed a start tag.");
                debug_assert_eq!(popped_tag.to_end(), tag);
                let (link_type, url, title) = match popped_tag {
                    Tag::Link {
                        link_type,
                        dest_url,
                        title,
                        ..
                    }
                    | Tag::Image {
                        link_type,
                        dest_url,
                        title,
                        ..
                    } => (link_type, dest_url, title),
                    _ => unreachable!("Should reach the end of a corresponding tag."),
                };

                let text = &self.input[range.clone()];

                match link_type {
                    LinkType::Inline => {
                        if let Some((source_url, title_and_quote)) =
                            crate::links::find_inline_url_and_title(text)
                        {
                            self.write_inline_link(&source_url, title_and_quote)?;
                        } else {
                            let title = if title.is_empty() {
                                None
                            } else {
                                Some((title, '"'))
                            };
                            self.write_inline_link(&url, title)?;
                        }
                    }
                    LinkType::Reference | LinkType::ReferenceUnknown => {
                        let label = crate::links::find_reference_link_label(text);
                        write!(self, "][{label}]")?;
                    }
                    LinkType::Collapsed | LinkType::CollapsedUnknown => write!(self, "][]")?,
                    LinkType::Shortcut | LinkType::ShortcutUnknown => write!(self, "]")?,
                    LinkType::Autolink | LinkType::Email => write!(self, ">")?,
                }
            }
            TagEnd::Table => {
                let popped_tag = self.nested_context.pop();
                debug_assert_eq!(popped_tag.unwrap().to_end(), tag);
                if let Some(state) = self.table_state.take() {
                    self.join_with_indentation(&state.format()?, false)?;
                }
                let popped_indentation = self.indentation.pop().expect("we added `|` in start_tag");
                debug_assert_eq!(popped_indentation, "|");
            }
            TagEnd::TableRow | TagEnd::TableHead => {
                let popped_tag = self.nested_context.pop();
                debug_assert_eq!(popped_tag.unwrap().to_end(), tag);
            }
            TagEnd::TableCell => {
                if let Some(state) = self.table_state.as_mut() {
                    // We finished formatting this cell. Setup the state to format the next cell
                    state.increment_col_index()
                }
            }
            TagEnd::HtmlBlock => {
                self.flush_external_formatted()?;
                self.check_needs_indent(&Event::End(tag));
            }
            TagEnd::MetadataBlock(kind) => {
                self.write_metadata_block_separator(&kind, range)?;
            }
        }
        Ok(())
    }

    fn new_external_formatted(
        &mut self,
        buffer_type: BufferType,
        capacity: usize,
    ) -> std::fmt::Result {
        self.flush_external_formatted()?;
        self.external_formatter = Some(E::new(buffer_type, self.formatter_width(), capacity));
        Ok(())
    }

    fn flush_external_formatted(&mut self) -> std::fmt::Result {
        if let Some(external_formatter) = self.external_formatter.take() {
            tracing::debug!("Flushing external formatter.");
            let start_with_indentation =
                !matches!(external_formatter.context(), FormattingContext::Paragraph);
            match (start_with_indentation, self.rewrite_buffer.chars().last()) {
                (false, _) | (_, Some('\n') | None) => {}
                // Code and HTML blocks should start have a `\n` before them.
                _ => self.write_str("\n")?,
            }
            self.join_with_indentation(&external_formatter.into_buffer(), start_with_indentation)?;
        }
        Ok(())
    }

    fn write_emphasis_marker(&mut self, range: &Range<usize>) -> std::fmt::Result {
        match self.config.fixed_emphasis_marker {
            None => rewrite_marker_with_limit(self.input, range, self, Some(1)),
            Some(marker) => self.write_str(marker),
        }
    }

    fn write_strong_marker(&mut self, range: &Range<usize>) -> std::fmt::Result {
        match self.config.fixed_strong_marker {
            None => rewrite_marker_with_limit(self.input, range, self, Some(2)),
            Some(marker) => self.write_str(marker),
        }
    }

    fn write_metadata_block_separator(
        &mut self,
        kind: &MetadataBlockKind,
        range: Range<usize>,
    ) -> std::fmt::Result {
        let newlines = self.count_newlines(&range);
        self.write_newlines(newlines)?;
        let marker = match kind {
            MetadataBlockKind::YamlStyle => "---",
            MetadataBlockKind::PlusesStyle => "+++",
        };
        writeln!(self, "{marker}")
    }
}

/// Count the number of `\n` in a snippet.
fn count_newlines(snippet: &str) -> usize {
    snippet.chars().filter(|char| *char == '\n').count()
}

/// Find some marker that denotes the start of a markdown construct.
/// for example, `**` for bold or `_` for italics.
fn find_marker<'i, P>(input: &'i str, range: &Range<usize>, predicate: P) -> &'i str
where
    P: FnMut(char) -> bool,
{
    let end = if let Some(position) = input[range.start..].chars().position(predicate) {
        range.start + position
    } else {
        range.end
    };
    &input[range.start..end]
}

/// Find some marker, but limit the size
fn rewrite_marker_with_limit<W: std::fmt::Write>(
    input: &str,
    range: &Range<usize>,
    writer: &mut W,
    size_limit: Option<usize>,
) -> std::fmt::Result {
    let marker_char = input[range.start..].chars().next().unwrap();
    let marker = find_marker(input, range, |c| c != marker_char);
    if let Some(mark_max_width) = size_limit {
        write!(writer, "{}", &marker[..mark_max_width])
    } else {
        write!(writer, "{marker}")
    }
}

/// Finds a marker in the source text and writes it to the buffer
fn rewrite_marker<W: std::fmt::Write>(
    input: &str,
    range: &Range<usize>,
    writer: &mut W,
) -> std::fmt::Result {
    rewrite_marker_with_limit(input, range, writer, None)
}

/// Rewrite a list of h1, h2, h3, h4, h5, h6 classes
fn rewirte_header_classes(classes: Vec<CowStr>) -> Result<String, std::fmt::Error> {
    let item_len = classes.iter().map(|i| i.len()).sum::<usize>();
    let capacity = item_len + classes.len() * 2;
    let mut result = String::with_capacity(capacity);
    for class in classes {
        write!(result, " .{class}")?;
    }
    Ok(result)
}

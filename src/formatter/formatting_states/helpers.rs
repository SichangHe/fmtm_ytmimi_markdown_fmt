use super::*;

impl<'i, E, I> FormatState<'i, E, I>
where
    E: ExternalFormatter,
    I: Iterator<Item = (Event<'i>, std::ops::Range<usize>)>,
{
    pub(crate) fn formatter_width(&self) -> Option<usize> {
        self.config
            .max_width
            .map(|w| w.saturating_sub(self.indentation_len()))
    }

    /// Peek at the next Markdown Event
    pub(crate) fn peek(&mut self) -> Option<&Event<'i>> {
        self.events.peek().map(|(e, _)| e)
    }

    /// Peek at the next Markdown Event and it's original position in the input
    pub(crate) fn peek_with_range(&mut self) -> Option<(&Event, &Range<usize>)> {
        self.events.peek().map(|(e, r)| (e, r))
    }

    /// Check if the next Event is an `Event::End`
    pub(crate) fn is_next_end_event(&mut self) -> bool {
        matches!(self.peek(), Some(Event::End(_)))
    }

    /// Check if we should write newlines and indentation before the next Start Event
    pub(crate) fn check_needs_indent(&mut self, event: &Event<'i>) {
        self.needs_indent = match self.peek() {
            Some(Event::Start(_) | Event::Rule | Event::Html(_) | Event::End(TagEnd::Item)) => true,
            Some(Event::End(TagEnd::BlockQuote)) => matches!(event, Event::End(_)),
            Some(Event::Text(_)) => matches!(event, Event::End(_) | Event::Start(Tag::Item)),
            _ => matches!(event, Event::Rule),
        };
    }

    /// Check if we're formatting a fenced code block
    pub(crate) fn in_fenced_code_block(&self) -> bool {
        matches!(
            self.nested_context.last(),
            Some(Tag::CodeBlock(CodeBlockKind::Fenced(_)))
        )
    }

    /// Check if we're formatting an indented code block
    pub(crate) fn in_indented_code_block(&self) -> bool {
        matches!(
            self.nested_context.last(),
            Some(Tag::CodeBlock(CodeBlockKind::Indented))
        )
    }

    /// Check if we're in an HTML block.
    pub(crate) fn in_html_block(&self) -> bool {
        Some(FormattingContext::HtmlBlock) == self.external_formatter.as_ref().map(|f| f.context())
    }

    // check if we're formatting a table header
    pub(crate) fn in_table_header(&self) -> bool {
        self.nested_context
            .iter()
            .rfind(|tag| **tag == Tag::TableHead)
            .is_some()
    }

    // check if we're formatting a table row
    pub(crate) fn in_table_row(&self) -> bool {
        self.nested_context
            .iter()
            .rfind(|tag| **tag == Tag::TableRow)
            .is_some()
    }

    /// Check if we're formatting a link
    pub(crate) fn in_link_or_image(&self) -> bool {
        matches!(
            self.nested_context.last(),
            Some(Tag::Link { .. } | Tag::Image { .. })
        )
    }

    /// Check if we're in a "paragraph". A `Paragraph` might not necessarily be on the
    /// nested_context stack.
    pub(crate) fn in_paragraph(&self) -> bool {
        Some(FormattingContext::Paragraph) == self.external_formatter.as_ref().map(|f| f.context())
    }

    /// Check if we're formatting in a nested context
    pub(crate) fn is_nested(&self) -> bool {
        !self.nested_context.is_empty()
    }

    /// Get the length of the indentation
    pub(crate) fn indentation_len(&self) -> usize {
        self.indentation.iter().map(|i| i.len()).sum()
    }

    /// Get an exclusive reference to the current buffer we're writing to. That could be the main
    /// rewrite buffer, the code block buffer, the internal table state, or anything else we're
    /// writing to while reformatting
    pub(crate) fn current_buffer(&mut self) -> Option<&mut dyn std::fmt::Write> {
        if self.force_rewrite_buffer {
            tracing::trace!("force_rewrite_buffer");
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
        } else if let Some(external_formatter) = self.external_formatter.as_mut() {
            tracing::trace!(context = ?external_formatter.context());
            Some(external_formatter as &mut dyn std::fmt::Write)
        } else {
            tracing::trace!("rewrite_buffer");
            Some(&mut self.rewrite_buffer)
        }
    }

    /// Check if the current buffer we're writting to is empty
    pub(crate) fn is_current_buffer_empty(&self) -> bool {
        if self.in_fenced_code_block() || self.in_indented_code_block() || self.in_html_block() {
            self.external_formatter
                .as_ref()
                .is_some_and(ExternalFormatter::is_empty)
        } else if self.in_table_header() || self.in_table_row() {
            self.table_state.as_ref().is_some_and(|s| s.is_empty())
        } else if let Some(external_formatter) = self.external_formatter.as_ref() {
            external_formatter.is_empty()
        } else {
            self.rewrite_buffer.is_empty()
        }
    }

    pub(crate) fn count_newlines(&self, range: &Range<usize>) -> usize {
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

    pub(crate) fn write_indentation(
        &mut self,
        trim_trailing_whiltespace: bool,
    ) -> std::fmt::Result {
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

    pub(crate) fn write_newlines(&mut self, max_newlines: usize) -> std::fmt::Result {
        self.write_newlines_inner(max_newlines, false)
    }

    pub(crate) fn write_newlines_no_trailing_whitespace(
        &mut self,
        max_newlines: usize,
    ) -> std::fmt::Result {
        self.write_newlines_inner(max_newlines, true)
    }

    pub(crate) fn write_newlines_inner(
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

    pub(crate) fn write_newlines_before_code_block(
        &mut self,
        newlines: usize,
    ) -> Result<bool, std::fmt::Error> {
        for _ in 0..newlines {
            self.write_char('\n')?;
        }
        self.write_indentation_if_needed()
    }

    pub(crate) fn write_newline_after_code_block(
        &mut self,
        empty_code_block: bool,
    ) -> std::fmt::Result {
        if !empty_code_block && !matches!(self.rewrite_buffer.chars().last(), Some('\n')) {
            tracing::trace!(r"Writing an extra `\n` after code block.");
            writeln!(self)?;
        }
        self.write_indentation(false)
    }

    pub(crate) fn write_indentation_if_needed(&mut self) -> Result<bool, std::fmt::Error> {
        match self.rewrite_buffer.chars().last() {
            Some('\n') | None => {
                self.write_indentation(false)?;
                Ok(true)
            }
            _ => Ok(false),
        }
    }

    pub(crate) fn write_reference_link_definition_inner(
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

    pub(crate) fn rewrite_reference_link_definitions(
        &mut self,
        range: &Range<usize>,
    ) -> std::fmt::Result {
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
    pub(crate) fn rewrite_final_reference_links(mut self) -> Result<String, std::fmt::Error> {
        // use std::mem::take to work around the borrow checker
        let reference_links = std::mem::take(&mut self.reference_links);
        tracing::trace!(?reference_links);

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

    pub(crate) fn join_with_indentation(
        &mut self,
        buffer: &str,
        start_with_indentation: bool,
        trim_last_newline: bool,
    ) -> std::fmt::Result {
        tracing::trace!(start_with_indentation, buffer);
        self.force_rewrite_buffer = true;
        let mut lines = buffer.split_inclusive('\n').peekable();
        while let Some(line) = lines.next() {
            let is_last = lines.peek().is_none();
            let is_next_empty = lines
                .peek()
                .map(|l| l.trim().is_empty())
                .unwrap_or_default();

            if start_with_indentation {
                self.write_indentation(line.trim().is_empty())?;
            }

            if line.trim().is_empty() {
                self.write_str(line.trim_start_matches(' '))?;
            } else if is_last && trim_last_newline {
                self.write_str(line.trim_end_matches('\n'))?;
            } else {
                self.write_str(line)?;
            }

            if !is_last && !start_with_indentation {
                self.write_indentation(is_next_empty)?;
            }
        }
        self.force_rewrite_buffer = false;
        Ok(())
    }

    pub(crate) fn new_external_formatted(
        &mut self,
        buffer_type: BufferType,
        capacity: usize,
    ) -> std::fmt::Result {
        self.flush_external_formatted(true)?;
        self.external_formatter = Some(E::new(buffer_type, self.formatter_width(), capacity));
        Ok(())
    }

    pub(crate) fn flush_external_formatted(&mut self, trim_last_newline: bool) -> std::fmt::Result {
        if let Some(external_formatter) = self.external_formatter.take() {
            tracing::debug!("Flushing external formatter.");
            let external = !matches!(external_formatter.context(), FormattingContext::Paragraph);
            match (external, self.rewrite_buffer.chars().last()) {
                (false, _) | (_, Some('\n' | ' ' | '$') | None) => {}
                // Code and HTML blocks should have a `\n` or some sort of
                // indentation before them.
                _ => self.write_str("\n")?,
            }
            self.join_with_indentation(
                &external_formatter.into_buffer(),
                self.needs_indent && external,
                trim_last_newline,
            )?;
        }
        Ok(())
    }

    pub(crate) fn write_emphasis_marker(&mut self, range: &Range<usize>) -> std::fmt::Result {
        match self.config.fixed_emphasis_marker {
            None => rewrite_marker_with_limit(self.input, range, self, Some(1)),
            Some(marker) => self.write_str(marker),
        }
    }

    pub(crate) fn write_strong_marker(&mut self, range: &Range<usize>) -> std::fmt::Result {
        match self.config.fixed_strong_marker {
            None => rewrite_marker_with_limit(self.input, range, self, Some(2)),
            Some(marker) => self.write_str(marker),
        }
    }

    pub(crate) fn write_metadata_block_separator(
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
pub(crate) fn count_newlines(snippet: &str) -> usize {
    snippet.chars().filter(|char| *char == '\n').count()
}

/// Find some marker that denotes the start of a markdown construct.
/// for example, `**` for bold or `_` for italics.
pub(crate) fn find_marker<'i, P>(input: &'i str, range: &Range<usize>, predicate: P) -> &'i str
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
pub(crate) fn rewrite_marker_with_limit<W: std::fmt::Write>(
    input: &str,
    range: &Range<usize>,
    writer: &mut W,
    size_limit: Option<usize>,
) -> std::fmt::Result {
    let marker_char = input[range.start..].chars().next().unwrap();
    let marker = find_marker(input, range, |c| c != marker_char);
    if let Some(mark_max_width) = size_limit {
        writer.write_str(&marker[..mark_max_width])
    } else {
        writer.write_str(marker)
    }
}

/// Finds a marker in the source text and writes it to the buffer
pub(crate) fn rewrite_marker<W: std::fmt::Write>(
    input: &str,
    range: &Range<usize>,
    writer: &mut W,
) -> std::fmt::Result {
    rewrite_marker_with_limit(input, range, writer, None)
}

/// Rewrite a list of h1, h2, h3, h4, h5, h6 classes
pub(crate) fn rewirte_header_classes(classes: Vec<CowStr>) -> Result<String, std::fmt::Error> {
    let item_len = classes.iter().map(|i| i.len()).sum::<usize>();
    let capacity = item_len + classes.len() * 2;
    let mut result = String::with_capacity(capacity);
    for class in classes {
        write!(result, " .{class}")?;
    }
    Ok(result)
}

use super::*;

impl<'i, E, I> FormatState<'i, E, I>
where
    I: Iterator<Item = (Event<'i>, std::ops::Range<usize>)>,
    E: ExternalFormatter,
{
    pub(crate) fn format_one_event(
        &mut self,
        event: Event<'i>,
        range: Range<usize>,
    ) -> std::fmt::Result {
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
            Event::Text(ref parsed_text) => {
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
            Event::DisplayMath(ref parsed_text) => {
                self.flush_external_formatted(false)?;
                self.write_str("$$")?;
                self.new_external_formatted(BufferType::DisplayMath, parsed_text.len())?;
                self.write_str(parsed_text)?;
                self.flush_external_formatted(false)?;
                self.write_indentation_if_needed()?;
                self.write_str("$$")?;
            }
            Event::Code(_) | Event::Html(_) => {
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
        self.last_position = last_position;
        Ok(())
    }

    pub(crate) fn start_tag(&mut self, tag: Tag<'i>, range: Range<usize>) -> std::fmt::Result {
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
                let info = match kind {
                    CodeBlockKind::Fenced(info_string) => {
                        self.write_newlines_before_code_block(newlines)?;
                        rewrite_marker(self.input, &range, self)?;

                        self.needs_indent = true;
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
                                .unwrap_or_else(|| info_string)
                                .trim()
                                .into();

                            if starts_with_space {
                                writeln!(self, " {info_string}")?;
                            } else {
                                writeln!(self, "{info_string}")?;
                            }
                            Some(info_string)
                        }
                    }
                    CodeBlockKind::Indented => {
                        // TODO(ytmimi) support tab as an indent
                        let indentation = "    ";
                        self.indentation.push(indentation.into());
                        if !matches!(self.peek(), Some(Event::End(TagEnd::CodeBlock))) {
                            // Only write the new line before and
                            // the indentation if
                            // this isn't an empty indented code block
                            if !self.write_newlines_before_code_block(newlines)? {
                                self.write_str(indentation)?;
                            }
                        }
                        self.needs_indent = false;
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
                let indentation = match self.peek() {
                    Some(Event::Start(Tag::CodeBlock(_) | Tag::HtmlBlock | Tag::TableHead)) => {
                        // Have to use the "correct" indentation if
                        // a code block, HTML block,
                        // or table follows immediately.
                        list_marker.indentation()
                    }
                    _ => self
                        .config
                        .fixed_indentation
                        .clone()
                        .unwrap_or_else(|| list_marker.indentation()),
                };
                self.indentation.push(indentation);
                // TODO(ytmimi) Add a configuration to allow incrementing ordered lists
                // list_marker.increment_count();
                // self.list_markers.push(list_marker)
            }
            Tag::FootnoteDefinition(label) => {
                let newlines = self.count_newlines(&range);
                self.write_newlines(newlines)?;
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
                self.flush_external_formatted(false)?;
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

    pub(crate) fn end_tag(&mut self, tag: TagEnd, range: Range<usize>) -> std::fmt::Result {
        match tag {
            TagEnd::Paragraph => {
                let popped_tag = self.nested_context.pop();
                debug_assert_eq!(popped_tag, Some(Tag::Paragraph));
                self.flush_external_formatted(true)?;
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
                self.flush_external_formatted(true)?;

                let popped_tag = self.nested_context.pop();
                let Some(Tag::CodeBlock(kind)) = &popped_tag else {
                    unreachable!("Should have pushed a code block start tag");
                };
                match kind {
                    CodeBlockKind::Fenced(_) => {
                        // write closing code fence
                        self.write_newline_after_code_block(empty_code_block)?;
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
                    self.join_with_indentation(&state.format()?, false, true)?;
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
                self.flush_external_formatted(true)?;
                self.check_needs_indent(&Event::End(tag));
            }
            TagEnd::MetadataBlock(kind) => {
                self.write_metadata_block_separator(&kind, range)?;
            }
        }
        Ok(())
    }
}

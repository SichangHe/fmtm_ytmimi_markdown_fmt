use super::*;

/// A default [`ExternalFormatter`].
/// Preserve code blocks as is,
/// trim indentation < 4 in display math and HTML blocks,
/// and line-wrap paragraphs.
pub type DefaultFormatterCombination =
    FormatterCombination<PreservingBuffer, TrimTo4Indent, TrimTo4Indent, Paragraph>;

/// A buffer where we write HTML blocks. Preserves everything as is.
pub struct PreservingBuffer {
    buffer: String,
    context: FormattingContext,
}

impl Write for PreservingBuffer {
    fn write_str(&mut self, s: &str) -> std::fmt::Result {
        self.buffer.push_str(s);
        Ok(())
    }
}

impl ExternalFormatter for PreservingBuffer {
    fn new(buffer_type: BufferType, _max_width: Option<usize>, capacity: usize) -> Self {
        tracing::trace!(?buffer_type, capacity, "PreservingBuffer::new");
        Self {
            buffer: String::with_capacity(capacity),
            context: buffer_type.to_formatting_context(),
        }
    }

    fn is_empty(&self) -> bool {
        self.buffer.is_empty()
    }

    fn context(&self) -> FormattingContext {
        self.context
    }

    fn into_buffer(self) -> String {
        self.buffer
    }
}

const MARKDOWN_HARD_BREAK: &str = "  \n";

/// A buffer where we write text
pub struct Paragraph {
    buffer: String,
    max_width: Option<usize>,
}

impl Write for Paragraph {
    fn write_str(&mut self, s: &str) -> std::fmt::Result {
        let is_hard_break = |s: &str| -> bool {
            // Hard breaks can have any amount of leading whitesace followed by a newline
            s.strip_prefix("  ")
                .is_some_and(|maybe_hard_break| maybe_hard_break.trim_start_matches(' ').eq("\n"))
        };

        if self.max_width.is_some() && is_hard_break(s) {
            self.buffer.push_str(MARKDOWN_HARD_BREAK);
            return Ok(());
        }

        if self.max_width.is_some() && s.trim().is_empty() {
            // If the user configured the max_width then push a space so we can reflow text
            self.buffer.push(' ');
        } else {
            self.buffer.push_str(s);
        }

        Ok(())
    }
}

impl ExternalFormatter for Paragraph {
    fn new(_: BufferType, max_width: Option<usize>, capacity: usize) -> Self {
        tracing::trace!(max_width, capacity, "Paragraph::new");
        Self {
            max_width,
            buffer: String::with_capacity(capacity),
        }
    }

    fn is_empty(&self) -> bool {
        self.buffer.is_empty()
    }

    fn context(&self) -> FormattingContext {
        FormattingContext::Paragraph
    }

    fn into_buffer(mut self) -> String {
        let rewrite_buffer = std::mem::take(&mut self.buffer);

        let Some(max_width) = self.max_width else {
            // We didn't configure a max_width, so just return the buffer
            return rewrite_buffer;
        };

        let all_lines_with_max_width = rewrite_buffer.lines().all(|l| l.len() <= max_width);

        if all_lines_with_max_width {
            // Don't need to wrap any lines
            return rewrite_buffer;
        }

        let mut output_buffer = String::with_capacity(rewrite_buffer.capacity());

        let wrap_options = TextWrapOptions::new(max_width)
            .break_words(false)
            .word_separator(textwrap::WordSeparator::AsciiSpace)
            .wrap_algorithm(textwrap::WrapAlgorithm::FirstFit);

        let mut split_on_hard_breaks = rewrite_buffer.split(MARKDOWN_HARD_BREAK).peekable();

        while let Some(text) = split_on_hard_breaks.next() {
            let has_next = split_on_hard_breaks.peek().is_some();
            let wrapped_text = textwrap::fill(text, wrap_options.clone());
            output_buffer.push_str(&wrapped_text);
            if has_next {
                output_buffer.push_str(MARKDOWN_HARD_BREAK);
            }
        }

        output_buffer
    }
}

/// A buffer that trims each line's leading spaces down to a multiple of 4.
pub struct TrimTo4Indent {
    buffer: String,
    context: FormattingContext,
}

impl Write for TrimTo4Indent {
    fn write_str(&mut self, s: &str) -> std::fmt::Result {
        for line in s.split_inclusive('\n') {
            let line = match self.buffer.chars().last() {
                Some('\n') | None => {
                    match line.chars().take_while(|char| *char == ' ').count() % 4 {
                        n_insignificant_space if n_insignificant_space > 0 => {
                            tracing::trace!(?n_insignificant_space, line);
                            &line[n_insignificant_space..]
                        }
                        _ => line,
                    }
                }
                _ => line,
            };
            self.buffer.push_str(line);
        }
        Ok(())
    }
}

impl ExternalFormatter for TrimTo4Indent {
    fn new(buffer_type: BufferType, _max_width: Option<usize>, capacity: usize) -> Self {
        tracing::trace!(?buffer_type, capacity, "TrimStartBuffer::new");
        Self {
            buffer: String::with_capacity(capacity),
            context: buffer_type.to_formatting_context(),
        }
    }

    fn is_empty(&self) -> bool {
        self.buffer.is_empty()
    }

    fn context(&self) -> FormattingContext {
        self.context
    }

    fn into_buffer(self) -> String {
        self.buffer
    }
}

use super::*;

/// A buffer where we write HTML blocks. Preserves everything as is.
pub struct PreservingHtmlBlock {
    buffer: String,
    context: FormattingContext,
}

impl Write for PreservingHtmlBlock {
    fn write_str(&mut self, s: &str) -> std::fmt::Result {
        self.buffer.push_str(s);
        Ok(())
    }
}

impl ExternalFormatter for PreservingHtmlBlock {
    fn new(buffer_type: BufferType, _max_width: Option<usize>, capacity: usize) -> Self {
        Self {
            buffer: String::with_capacity(capacity),
            context: match buffer_type {
                BufferType::CodeBlock { .. } => FormattingContext::CodeBlock,
                BufferType::HtmlBlock => FormattingContext::HtmlBlock,
                BufferType::Paragraph => FormattingContext::Paragraph,
            },
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

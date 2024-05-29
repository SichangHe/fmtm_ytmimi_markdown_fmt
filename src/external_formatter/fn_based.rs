use super::*;

/// A formatting function `F` that takes the buffer type, optional maximum width,
/// and the string to format, and returns the formatted string.
pub trait FormatterFn: Default {
    /// Format the input string based on the configuration.
    fn format(
        &mut self,
        buffer_type: BufferType,
        max_width: Option<usize>,
        input: String,
    ) -> String;
}

/// A convenience function-based formatter.
/// Implement a single function [`FormatterFn::format`] and
/// set it as the generic parameter `F` to create a [`ExternalFormatter`].
pub struct FnFormatter<F>
where
    F: FormatterFn,
{
    buffer: String,
    buffer_type: BufferType<'static>,
    max_width: Option<usize>,
    formatter_fn: F,
}

impl<F> Write for FnFormatter<F>
where
    F: FormatterFn,
{
    fn write_str(&mut self, s: &str) -> std::fmt::Result {
        self.buffer.push_str(s);
        Ok(())
    }
}

impl<F> ExternalFormatter for FnFormatter<F>
where
    F: FormatterFn,
{
    fn new(buffer_type: BufferType, max_width: Option<usize>, capacity: usize) -> Self {
        let buffer_type = match buffer_type {
            BufferType::CodeBlock { info } => BufferType::CodeBlock {
                info: info.map(|info| info.to_string().into()),
            },
            BufferType::DisplayMath => BufferType::DisplayMath,
            BufferType::HtmlBlock => BufferType::HtmlBlock,
            BufferType::Paragraph => BufferType::Paragraph,
        };
        Self {
            buffer: String::with_capacity(capacity),
            buffer_type,
            max_width,
            formatter_fn: F::default(),
        }
    }

    fn is_empty(&self) -> bool {
        self.buffer.is_empty()
    }

    fn context(&self) -> FormattingContext {
        self.buffer_type.to_formatting_context()
    }

    fn into_buffer(mut self) -> String {
        self.formatter_fn
            .format(self.buffer_type, self.max_width, self.buffer)
    }
}

use super::*;

mod default;

pub use default::{DefaultFormatterCombination, PreservingBuffer, Paragraph};

/// A formatter buffer we write non-Markdown string into.
pub trait ExternalFormatter: Write {
    /// Make a new instance based on the given [`BufferType`], maximum width,
    /// and buffer capacity.
    fn new(buffer_type: BufferType, max_width: Option<usize>, capacity: usize) -> Self;

    /// Check if the internal buffer is empty.
    fn is_empty(&self) -> bool;

    /// Check what type of context this formatter is in.
    fn context(&self) -> FormattingContext;

    /// Consume Self and return the formatted buffer.
    fn into_buffer(self) -> String;
}

/// Type of the string being written to a [`ExternalFormatter`].
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum BufferType<'a> {
    /// String in a code block.
    CodeBlock {
        /// Optional [`info string`] of the code block.
        ///
        /// [`info string`]: https://spec.commonmark.org/0.31.2/#fenced-code-blocks
        info: Option<&'a str>,
    },
    /// String in an HTML block.
    HtmlBlock,
    /// String in a paragraph.
    Paragraph,
    // TODO: Math.
}

/// Type of the formatting context an [`ExternalFormatter`] is in.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum FormattingContext {
    /// A code block.
    CodeBlock,
    /// An HTML block.
    HtmlBlock,
    /// A paragraph.
    Paragraph,
    // TODO: Math.
}

/// A convenience combination of
/// external formatters implementing [`ExternalFormatter`],
/// using one [`ExternalFormatter`] for each of code block, HTML block,
/// and paragraph formatting.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum FormatterCombination<C, H, P> {
    /// Inner code block formatter.
    CodeBlock(C),
    /// Inner HTML block formatter.
    HtmlBlock(H),
    /// Inner paragraph formatter.
    Paragraph(P),
    // TODO: Math.
}

impl<C, H, P> Write for FormatterCombination<C, H, P>
where
    C: Write,
    H: Write,
    P: Write,
{
    fn write_str(&mut self, s: &str) -> std::fmt::Result {
        match self {
            Self::CodeBlock(c) => c.write_str(s),
            Self::HtmlBlock(h) => h.write_str(s),
            Self::Paragraph(p) => p.write_str(s),
        }
    }
}

impl<C, H, P> ExternalFormatter for FormatterCombination<C, H, P>
where
    C: ExternalFormatter,
    H: ExternalFormatter,
    P: ExternalFormatter,
{
    fn new(buffer_type: BufferType, max_width: Option<usize>, capacity: usize) -> Self {
        match buffer_type {
            BufferType::CodeBlock { .. } => {
                Self::CodeBlock(C::new(buffer_type, max_width, capacity))
            }
            BufferType::HtmlBlock => Self::HtmlBlock(H::new(buffer_type, max_width, capacity)),
            BufferType::Paragraph => Self::Paragraph(P::new(buffer_type, max_width, capacity)),
        }
    }

    fn is_empty(&self) -> bool {
        match self {
            Self::CodeBlock(c) => c.is_empty(),
            Self::HtmlBlock(h) => h.is_empty(),
            Self::Paragraph(p) => p.is_empty(),
        }
    }

    fn context(&self) -> FormattingContext {
        match self {
            Self::CodeBlock(c) => c.context(),
            Self::HtmlBlock(h) => h.context(),
            Self::Paragraph(p) => p.context(),
        }
    }

    fn into_buffer(self) -> String {
        match self {
            Self::CodeBlock(c) => c.into_buffer(),
            Self::HtmlBlock(h) => h.into_buffer(),
            Self::Paragraph(p) => p.into_buffer(),
        }
    }
}

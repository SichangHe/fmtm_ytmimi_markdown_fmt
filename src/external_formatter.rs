use super::*;

mod default;
mod fn_based;

pub use {
    default::{DefaultFormatterCombination, Paragraph, PreservingBuffer, TrimTo4Indent},
    fn_based::{FnFormatter, FormatterFn},
};

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
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum BufferType<'a> {
    /// String in a code block.
    CodeBlock {
        /// Optional [`info string`] of the code block.
        ///
        /// [`info string`]: https://spec.commonmark.org/0.31.2/#fenced-code-blocks
        info: Option<CowStr<'a>>,
    },
    /// Display math expression.
    DisplayMath,
    /// String in an HTML block.
    HtmlBlock,
    /// String in a paragraph.
    Paragraph,
}

impl<'a> BufferType<'a> {
    /// The associated [`FormattingContext`] of this buffer type.
    pub fn to_formatting_context(&self) -> FormattingContext {
        match self {
            Self::CodeBlock { .. } => FormattingContext::CodeBlock,
            Self::DisplayMath => FormattingContext::DisplayMath,
            Self::HtmlBlock => FormattingContext::HtmlBlock,
            Self::Paragraph => FormattingContext::Paragraph,
        }
    }
}

/// Type of the formatting context an [`ExternalFormatter`] is in.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum FormattingContext {
    /// A code block.
    CodeBlock,
    /// A display math block.
    DisplayMath,
    /// An HTML block.
    HtmlBlock,
    /// A paragraph.
    Paragraph,
}

/// A convenience combination of
/// external formatters implementing [`ExternalFormatter`],
/// using one [`ExternalFormatter`] for each of code block (`C`),
/// display math (`D`), HTML block (`H`), and paragraph (`P`) formatting.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum FormatterCombination<C, D, H, P> {
    /// Inner code block formatter.
    CodeBlock(C),
    /// Inner display math formatter.
    DisplayMath(D),
    /// Inner HTML block formatter.
    HtmlBlock(H),
    /// Inner paragraph formatter.
    Paragraph(P),
}

impl<C, D, H, P> Write for FormatterCombination<C, D, H, P>
where
    C: Write,
    D: Write,
    H: Write,
    P: Write,
{
    fn write_str(&mut self, s: &str) -> std::fmt::Result {
        match self {
            Self::CodeBlock(c) => c.write_str(s),
            Self::DisplayMath(d) => d.write_str(s),
            Self::HtmlBlock(h) => h.write_str(s),
            Self::Paragraph(p) => p.write_str(s),
        }
    }
}

impl<C, D, H, P> ExternalFormatter for FormatterCombination<C, D, H, P>
where
    C: ExternalFormatter,
    D: ExternalFormatter,
    H: ExternalFormatter,
    P: ExternalFormatter,
{
    fn new(buffer_type: BufferType, max_width: Option<usize>, capacity: usize) -> Self {
        match buffer_type {
            BufferType::CodeBlock { .. } => {
                Self::CodeBlock(C::new(buffer_type, max_width, capacity))
            }
            BufferType::DisplayMath => Self::DisplayMath(D::new(buffer_type, max_width, capacity)),
            BufferType::HtmlBlock => Self::HtmlBlock(H::new(buffer_type, max_width, capacity)),
            BufferType::Paragraph => Self::Paragraph(P::new(buffer_type, max_width, capacity)),
        }
    }

    fn is_empty(&self) -> bool {
        match self {
            Self::CodeBlock(c) => c.is_empty(),
            Self::DisplayMath(d) => d.is_empty(),
            Self::HtmlBlock(h) => h.is_empty(),
            Self::Paragraph(p) => p.is_empty(),
        }
    }

    fn context(&self) -> FormattingContext {
        match self {
            Self::CodeBlock(c) => c.context(),
            Self::DisplayMath(d) => d.context(),
            Self::HtmlBlock(h) => h.context(),
            Self::Paragraph(p) => p.context(),
        }
    }

    fn into_buffer(self) -> String {
        match self {
            Self::CodeBlock(c) => c.into_buffer(),
            Self::DisplayMath(d) => d.into_buffer(),
            Self::HtmlBlock(h) => h.into_buffer(),
            Self::Paragraph(p) => p.into_buffer(),
        }
    }
}

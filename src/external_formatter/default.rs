use super::*;

/// A default [`ExternalFormatter`].
/// Preserve code blocks and HTML blocks as is, and line-wrap paragraphs.
pub type DefaultFormatterCombination =
    FormatterCombination<PreservingHtmlBlock, PreservingHtmlBlock, Paragraph>;

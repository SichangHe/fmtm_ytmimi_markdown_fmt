//! Easily format Markdown.
//! [fmtm_ytmimi_markdown_fmt] supports [CommonMark] and [GitHub Flavored Markdown].
//!
//! [fmtm_ytmimi_markdown_fmt]: index.html
//! [CommonMark]: https://spec.commonmark.org/
//! [GitHub Flavored Markdown]: https://github.github.com/gfm/
//!
//! # Getting Started
//!
//! ```rust
//! use fmtm_ytmimi_markdown_fmt::MarkdownFormatter;
//!
//! let markdown = r##" # Getting Started
//! 1. numbered lists
//! 1.  are easy!
//! "##;
//!
//! let formatted = r##"# Getting Started
//! 1. numbered lists
//! 1. are easy!
//! "##;
//!
//! let output = MarkdownFormatter::default().format(markdown)?;
//! # assert_eq!(output, formatted);
//! # Ok::<(), std::fmt::Error>(())
//! ```
//!
//! # Using [`MarkdownFormatter`] as a builder
//!
//! The formatter gives you control to configure Markdown formatting.
//!
// TODO: Fix this example.
// //! ````rust
// //! use fmtm_ytmimi_markdown_fmt::{
// //!     rewrite_markdown, rewrite_markdown_with_builder, MarkdownFormatter,
// //! };
// //!
// //! let builder = MarkdownFormatter::with_code_block_formatter(|info_string, code_block| {
// //!     match info_string.to_lowercase().as_str() {
// //!         "markdown" => rewrite_markdown(&code_block).unwrap_or(code_block),
// //!         _ => code_block
// //!     }
// //! });
// //!
// //! let markdown = r##" # Using the Builder
// //! + markdown code block nested in a list
// //!   ```markdown
// //!   A nested markdown snippet!
// //!
// //!    * unordered lists
// //!    are also pretty easy!
// //!    - `-` or `+` can also be used as unordered list markers.
// //!    ```
// //! "##;
// //!
// //! let formatted = r##"# Using the Builder
// //! + markdown code block nested in a list
// //!   ```markdown
// //!   A nested markdown snippet!
// //!
// //!   * unordered lists
// //!     are also pretty easy!
// //!   - `-` or `+` can also be used as unordered list markers.
// //!   ```
// //! "##;
// //!
// //! let output = rewrite_markdown_with_builder(markdown, builder)?;
// //! # assert_eq!(output, formatted);
// //! # Ok::<(), std::fmt::Error>(())
// //! ````

use std::borrow::Cow;
use std::collections::VecDeque;
use std::fmt::Write;
use std::iter::Peekable;
use std::num::ParseIntError;
use std::ops::Range;
use std::str::FromStr;

use itertools::{EitherOrBoth, Itertools};
use pulldown_cmark::{
    Alignment, CodeBlockKind, CowStr, Event, HeadingLevel, LinkType, Options, Parser, Tag, TagEnd,
};
use textwrap::Options as TextWrapOptions;
use unicode_segmentation::UnicodeSegmentation;

mod adapters;
mod builder;
mod config;
mod escape;
mod external_formatter;
mod formatter;
mod links;
pub mod list;
mod table;
#[cfg(test)]
mod test;
mod utils;

use crate::{
    adapters::LooseListExt, formatter::FormatState, table::TableState, utils::unicode_str_width,
};
pub use crate::{
    builder::MarkdownFormatter,
    config::Config,
    external_formatter::{
        BufferType, DefaultFormatterCombination, ExternalFormatter, FormatterCombination,
        FormattingContext, Paragraph, PreservingBuffer, TrimStartBuffer,
    },
    list::{ListMarker, OrderedListMarker, ParseListMarkerError, UnorderedListMarker},
};

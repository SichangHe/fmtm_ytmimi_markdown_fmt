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
//! let expected = r##"# Getting Started
//! 1. numbered lists
//! 1. are easy!
//! "##;
//!
//! let output = MarkdownFormatter::default().format(markdown)?;
//! assert_eq!(output, expected);
//! # Ok::<(), std::fmt::Error>(())
//! ```
//!
//! # Using [`MarkdownFormatter`] as a builder
//!
//! The formatter gives you control to configure Markdown formatting.
//! ````rust
//! use fmtm_ytmimi_markdown_fmt::*;
//! #[derive(Default)]
//! struct CodeBlockFormatter;
//! impl FormatterFn for CodeBlockFormatter {
//!     fn format(
//!         &mut self,
//!         buffer_type: BufferType,
//!         _max_width: Option<usize>,
//!         input: String,
//!     ) -> String {
//!         let BufferType::CodeBlock { info } = buffer_type else {
//!             unreachable!();
//!         };
//!         match info {
//!             Some(info) if info.as_ref() == "markdown" => {
//!                 MarkdownFormatter::default().format(&input).unwrap_or(input)
//!             }
//!             _ => input,
//!         }
//!     }
//! }
//!
//! let input = r##" # Using the Builder
//! + markdown code block nested in a list
//!   ```markdown
//!   A nested markdown snippet!
//!
//!    * unordered lists
//!    are also pretty easy!
//!    - `-` or `+` can also be used as unordered list markers.
//!    ```
//! "##;
//!
//! let expected = r##"# Using the Builder
//! - markdown code block nested in a list
//!     ```markdown
//!     A nested markdown snippet!
//!
//!     * unordered lists
//!       are also pretty easy!
//!     - `-` or `+` can also be used as unordered list markers.
//!     ```
//! "##;
//!
//! type MyFormatter = MarkdownFormatter<
//!     FormatterCombination<
//!         FnFormatter<CodeBlockFormatter>,
//!         TrimTo4Indent,
//!         TrimTo4Indent,
//!         Paragraph,
//!     >,
//! >;
//! let output =
//!     MyFormatter::with_config_and_external_formatter(Config::sichanghe_opinion()).format(input)?;
//! assert_eq!(output, expected);
//! # Ok::<(), std::fmt::Error>(())
//! ````

use std::{
    borrow::Cow, collections::VecDeque, fmt::Write, iter::Peekable, marker::PhantomData,
    num::ParseIntError, ops::Range, str::FromStr,
};

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
    adapters::{LooseListExt, SequentialBlockExt},
    formatter::FormatState,
    table::TableState,
    utils::unicode_str_width,
};
pub use crate::{
    builder::MarkdownFormatter,
    config::Config,
    external_formatter::{
        BufferType, DefaultFormatterCombination, ExternalFormatter, FnFormatter,
        FormatterCombination, FormatterFn, FormattingContext, Paragraph, PreservingBuffer,
        TrimTo4Indent,
    },
    list::{ListMarker, OrderedListMarker, ParseListMarkerError, UnorderedListMarker},
};

use super::*;

mod format;
mod helpers;

pub(crate) use helpers::*;

type ReferenceLinkDefinition = (String, String, Option<(String, char)>, Range<usize>);

pub(crate) struct FormatState<'i, E, I>
where
    E: ExternalFormatter,
    I: Iterator<Item = (Event<'i>, std::ops::Range<usize>)>,
{
    /// Raw markdown input
    input: &'i str,
    pub(crate) last_was_softbreak: bool,
    /// Iterator Supplying Markdown Events
    events: Peekable<I>,
    rewrite_buffer: String,
    /// Handles code block, HTML block, and paragraph formatting.
    external_formatter: Option<E>,
    /// Stack that keeps track of nested list markers.
    /// Unordered list markers are one of `*`, `+`, or `-`,
    /// while ordered lists markers start with 0-9 digits followed by a `.` or `)`.
    // TODO(ytmimi) Add a configuration to allow incrementing ordered lists
    // list_markers: Vec<ListMarker>,
    /// Stack that keeps track of indentation.
    indentation: Vec<Cow<'static, str>>,
    /// Stack that keeps track of whether we're formatting inside of another element.
    nested_context: Vec<Tag<'i>>,
    /// A set of reference link definitions that will be output after formatting.
    /// Reference style links contain 3 parts:
    /// 1. Text to display
    /// 2. URL
    /// 3. (Optional) Title
    /// ```markdown
    /// [title]: link "optional title"
    /// ```
    reference_links: Vec<ReferenceLinkDefinition>,
    /// keep track of the current setext header.
    /// ```markdown
    /// Header
    /// ======
    /// ```
    setext_header: Option<&'i str>,
    /// Store the fragment identifier and classes from the header start tag.
    header_id_and_classes: Option<(Option<CowStr<'i>>, Vec<CowStr<'i>>)>,
    /// next Start event should push indentation
    needs_indent: bool,
    table_state: Option<TableState<'i>>,
    last_position: usize,
    trim_link_or_image_start: bool,
    /// Force write into rewrite buffer.
    // TODO: Remove this after making an adapter to solve the stupid
    // out-of-order problem.
    force_rewrite_buffer: bool,
    /// Format configurations
    config: Config,
}

/// Depnding on the formatting context there are a few different buffers where we might want to
/// write formatted markdown events. The Write impl helps us centralize this logic.
impl<'i, E, I> Write for FormatState<'i, E, I>
where
    I: Iterator<Item = (Event<'i>, std::ops::Range<usize>)>,
    E: ExternalFormatter,
{
    fn write_str(&mut self, text: &str) -> std::fmt::Result {
        if let Some(writer) = self.current_buffer() {
            tracing::trace!(text, "write_str");
            writer.write_str(text)?
        }
        Ok(())
    }

    fn write_fmt(&mut self, args: std::fmt::Arguments<'_>) -> std::fmt::Result {
        if let Some(writer) = self.current_buffer() {
            writer.write_fmt(args)?
        }
        Ok(())
    }
}

impl<'i, E, I> FormatState<'i, E, I>
where
    I: Iterator<Item = (Event<'i>, std::ops::Range<usize>)>,
    E: ExternalFormatter,
{
    pub(crate) fn new(
        input: &'i str,
        config: Config,
        iter: I,
        reference_links: Vec<ReferenceLinkDefinition>,
    ) -> Self {
        Self {
            input,
            last_was_softbreak: false,
            events: iter.peekable(),
            rewrite_buffer: String::with_capacity(input.len() * 2),
            external_formatter: None,
            // TODO(ytmimi) Add a configuration to allow incrementing ordered lists
            // list_markers: vec![],
            indentation: vec![],
            nested_context: vec![],
            reference_links,
            setext_header: None,
            header_id_and_classes: None,
            needs_indent: false,
            table_state: None,
            last_position: 0,
            trim_link_or_image_start: false,
            force_rewrite_buffer: false,
            config,
        }
    }

    /// The main entry point for markdown formatting.
    pub fn format(mut self) -> Result<String, std::fmt::Error> {
        while let Some((event, range)) = self.events.next() {
            self.format_one_event(event, range)?;
        }
        debug_assert!(self.nested_context.is_empty());
        let trailing_newline = self.input.ends_with('\n');
        self.rewrite_final_reference_links().map(|mut output| {
            if trailing_newline {
                output.push('\n');
            }
            output
        })
    }
}

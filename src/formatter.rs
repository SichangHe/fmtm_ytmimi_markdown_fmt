use pulldown_cmark::MetadataBlockKind;

use super::*;

mod formatting_states;

pub(crate) use formatting_states::FormatState;

impl<E> MarkdownFormatter<E>
where
    E: ExternalFormatter,
{
    /// Format Markdown input
    ///
    /// ```rust
    /// # use fmtm_ytmimi_markdown_fmt::MarkdownFormatter;
    /// let formatter = MarkdownFormatter::default();
    /// let input = "   #  Header! ";
    /// let rewrite = formatter.format(input).unwrap();
    /// assert_eq!(rewrite, String::from("# Header!"));
    /// ```
    pub fn format(self, input: &str) -> Result<String, std::fmt::Error> {
        // callback that will always revcover broken links
        let mut callback = |broken_link| {
            tracing::trace!("found boken link: {broken_link:?}");
            Some(("".into(), "".into()))
        };

        let mut options = Options::all();
        options.remove(Options::ENABLE_SMART_PUNCTUATION);

        let parser = Parser::new_with_broken_link_callback(input, options, Some(&mut callback));

        // There can't be any characters besides spaces, tabs, or newlines after the title
        // See https://spec.commonmark.org/0.30/#link-reference-definition for the
        // definition and https://spec.commonmark.org/0.30/#example-209 as an example.
        //
        // It seems that `pulldown_cmark` sometimes parses titles when it shouldn't.
        // To work around edge cases where a paragraph starting with a quoted string might be
        // interpreted as a link title we check that only whitespace follows the title
        let is_false_title = |input: &str, span: Range<usize>| {
            input[span.end..]
                .chars()
                .take_while(|c| *c != '\n')
                .any(|c| !c.is_whitespace())
        };

        let reference_links = parser
            .reference_definitions()
            .iter()
            .sorted_by(|(_, link_a), (_, link_b)| {
                // We want to sort these in descending order based on the ranges
                // This creates a stack of reference links that we can pop off of.
                link_b.span.start.cmp(&link_a.span.start)
            })
            // TODO: Fix typo.
            .map(|(link_lable, link_def)| {
                let (dest, title, span) = (&link_def.dest, &link_def.title, &link_def.span);
                let full_link = &input[span.clone()];
                if title.is_some() && is_false_title(input, span.clone()) {
                    let end = input[span.clone()]
                        .find(dest.as_ref())
                        .map(|idx| idx + dest.len())
                        .unwrap_or(span.end);
                    return (
                        link_lable.to_string(),
                        dest.to_string(),
                        None,
                        span.start..end,
                    );
                }

                if let Some((url, title)) = links::recover_escaped_link_destination_and_title(
                    full_link,
                    link_lable,
                    title.is_some(),
                ) {
                    (link_lable.to_string(), url, title, span.clone())
                } else {
                    // Couldn't recover URL from source, just use what we've been given
                    (
                        link_lable.to_string(),
                        dest.to_string(),
                        title.clone().map(|s| (s.to_string(), '"')),
                        span.clone(),
                    )
                }
            })
            .collect::<Vec<_>>();

        let iter = parser
            .into_offset_iter()
            .all_loose_lists()
            .all_sequential_blocks();

        let fmt_state = <FormatState<E, _>>::new(input, self.config, iter, reference_links);
        fmt_state.format()
    }
}

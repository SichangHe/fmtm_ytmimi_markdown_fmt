use super::*;

/// Conveniently turn any iterator that returns ([Event], [Range]) into a
/// [`SequentialBlockAdapter`].
pub(crate) trait SequentialBlockExt<'input, I>
where
    I: Iterator<Item = (Event<'input>, Range<usize>)>,
{
    fn all_sequential_blocks(self) -> SequentialBlockAdapter<'input, I>;
}

// Blanket impl for all iterators
impl<'input, I> SequentialBlockExt<'input, I> for I
where
    I: Iterator<Item = (Event<'input>, Range<usize>)>,
{
    fn all_sequential_blocks(self) -> SequentialBlockAdapter<'input, I> {
        SequentialBlockAdapter::new(self)
    }
}

/// Workaround for [pulldown_cmark]'s weirdness where a block (code, HTML,
/// or paragraph) starts before the previous one ends.
pub(crate) struct SequentialBlockAdapter<'input, I>
where
    I: Iterator<Item = (Event<'input>, Range<usize>)>,
{
    /// Inner iterator that return Events
    inner: I,
    /// The kind of block we are in.
    context: Option<FormattingContext>,
    /// Events that appear too early.
    out_of_place_events: VecDeque<(Event<'input>, Range<usize>)>,
}

impl<'input, I> Iterator for SequentialBlockAdapter<'input, I>
where
    I: Iterator<Item = (Event<'input>, Range<usize>)>,
{
    type Item = (Event<'input>, Range<usize>);

    fn next(&mut self) -> Option<Self::Item> {
        let (event, range) = self
            .out_of_place_events
            .pop_front()
            .or_else(|| self.inner.next())?;
        tracing::debug!(?event, ?range);
        match &event {
            Event::Start(tag) => {
                if let Some(context) = context_of_tag(tag) {
                    match self.context {
                        None => self.context = Some(context),
                        Some(_) => {
                            // Entering a new context without exiting the
                            // current one.
                            self.out_of_place_events.push_back((event, range));
                            let (event, range) = self.exhaust_mismatching_context();
                            self.context = None;
                            return Some((event, range));
                        }
                    }
                }
            }
            Event::End(tag) => {
                if let context @ Some(_) = context_of_tag_end(*tag) {
                    debug_assert_eq!(self.context, context);
                    self.context = None;
                }
            }
            _ => {}
        }
        Some((event, range))
    }
}

fn context_of_tag(tag: &Tag) -> Option<FormattingContext> {
    match tag {
        Tag::CodeBlock(_) => Some(FormattingContext::CodeBlock),
        Tag::HtmlBlock => Some(FormattingContext::HtmlBlock),
        Tag::Paragraph => Some(FormattingContext::Paragraph),
        _ => None,
    }
}

fn context_of_tag_end(tag: TagEnd) -> Option<FormattingContext> {
    match tag {
        TagEnd::CodeBlock => Some(FormattingContext::CodeBlock),
        TagEnd::HtmlBlock => Some(FormattingContext::HtmlBlock),
        TagEnd::Paragraph => Some(FormattingContext::Paragraph),
        _ => None,
    }
}

impl<'input, I> SequentialBlockAdapter<'input, I>
where
    I: Iterator<Item = (Event<'input>, Range<usize>)>,
{
    pub(super) fn new(inner: I) -> Self {
        Self {
            inner,
            context: None,
            out_of_place_events: VecDeque::new(),
        }
    }

    /// Find and cache all [Event]s that are out of place,
    /// and return the next [Event] that is in place.
    ///
    /// Currently, do not handle the case where [Event::End] is missing.
    fn exhaust_mismatching_context(&mut self) -> (Event<'input>, Range<usize>) {
        for (event, range) in self.inner.by_ref() {
            match event {
                Event::End(tag) if context_of_tag_end(tag) == self.context => {
                    return (event, range);
                }
                _ => {
                    self.out_of_place_events.push_back((event, range));
                }
            }
        }
        tracing::error!(?self.context, ?self.out_of_place_events);
        panic!("No matching end tag found");
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn check_adapter_events() {
        // TODO: Write tests after making the other adapter use Insta.
    }
}

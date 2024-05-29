use super::*;

/// Used to format Markdown inputs.
///
/// Parameter `E` should be an [`ExternalFormatter`] to configure code block,
/// HTML block, and paragraph formatting;
/// default to [`DefaultFormatterCombination`],
/// and partial customization can easily be done using [`FormatterCombination`].
#[derive(Clone)]
pub struct MarkdownFormatter<E>
where
    E: ExternalFormatter,
{
    pub(crate) _external_formatter: PhantomData<fn() -> E>,
    pub(crate) config: Config,
}

impl MarkdownFormatter<DefaultFormatterCombination> {
    /// Create a [`MarkdownFormatter`] with custom [`Config`] and
    /// default [`ExternalFormatter`].
    ///
    /// ```rust
    /// # use fmtm_ytmimi_markdown_fmt::{Config, MarkdownFormatter};
    /// let formatter = MarkdownFormatter::with_config(Config {
    ///     max_width: Some(80),
    ///     ..Default::default()
    /// });
    /// ```
    pub fn with_config(config: Config) -> Self {
        Self {
            config,
            ..Default::default()
        }
    }
}

impl<E> MarkdownFormatter<E>
where
    E: ExternalFormatter,
{
    /// Create a [`MarkdownFormatter`] with custom [`Config`] and
    /// custom [`ExternalFormatter`].
    ///
    /// ```rust
    /// # use fmtm_ytmimi_markdown_fmt::{
    ///     Config, DefaultFormatterCombination, MarkdownFormatter,
    /// };
    /// let formatter = <MarkdownFormatter<DefaultFormatterCombination>>::with_config(
    ///     Config {
    ///         max_width: Some(80),
    ///         ..Default::default()
    ///     }
    /// );
    /// ```
    pub fn with_config_and_external_formatter(config: Config) -> Self {
        Self {
            _external_formatter: Default::default(),
            config,
        }
    }

    /// Configure the max with when rewriting paragraphs.
    ///
    /// When set to [None], the deafault, paragraph width is left unchanged.
    pub fn max_width(&mut self, max_width: Option<usize>) -> &mut Self {
        self.config.max_width = max_width;
        self
    }

    /// Set the configuration based on Steven Hé (Sīchàng)'s opinion.
    pub fn sichanghe_config(&mut self) -> &mut Self {
        self.config = Config::sichanghe_opinion();
        self
    }
}

impl<E> std::fmt::Debug for MarkdownFormatter<E>
where
    E: ExternalFormatter,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("MarkdownFormatter")
            .field("config", &self.config)
            .finish()
    }
}

impl Default for MarkdownFormatter<DefaultFormatterCombination> {
    fn default() -> Self {
        MarkdownFormatter {
            _external_formatter: Default::default(),
            config: Config::default(),
        }
    }
}

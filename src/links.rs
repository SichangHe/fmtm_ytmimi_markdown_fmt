use super::*;

impl<'i, E, I> FormatState<'i, E, I>
where
    E: ExternalFormatter,
    I: Iterator<Item = (Event<'i>, std::ops::Range<usize>)>,
{
    pub(super) fn write_inline_link<S: AsRef<str>>(
        &mut self,
        url: &str,
        title: Option<(S, char)>,
    ) -> std::fmt::Result {
        let url = format_link_url(url, false);
        match title {
            Some((title, ')')) => write!(self, r#"]({url} ({}))"#, title.as_ref())?,
            Some((title, quote)) => write!(self, r#"]({url} {quote}{}{quote})"#, title.as_ref())?,
            None => write!(self, "]({url})")?,
        }
        Ok(())
    }
}

pub(crate) fn format_link_url(url: &str, wrap_empty_urls: bool) -> Cow<'_, str> {
    if wrap_empty_urls && url.is_empty() {
        Cow::from("<>")
    } else if !url.starts_with('<') && !url.ends_with('>') && url.contains(' ')
        || !balanced_parens(url)
    {
        // https://spec.commonmark.org/0.30/#link-destination
        Cow::from(format!("<{url}>"))
    } else {
        url.into()
    }
}

/// Check if the parens are balanced
fn balanced_parens(url: &str) -> bool {
    let mut stack = vec![];
    let mut was_last_escape = false;

    for b in url.chars() {
        if !was_last_escape && b == '(' {
            stack.push(b);
            continue;
        }

        if !was_last_escape && b == ')' {
            if let Some(top) = stack.last() {
                if *top != '(' {
                    return false;
                }
                stack.pop();
            } else {
                return false;
            }
        }
        was_last_escape = b == '\\';
    }
    stack.is_empty()
}

/// Search for enclosing balanced brackets
fn find_text_within_last_set_of_balance_bracket(
    label: &str,
    opener: char,
    closer: char,
    halt_condition: Option<fn(char) -> bool>,
) -> (usize, usize) {
    let mut stack = vec![];
    let mut was_last_escape = false;

    let mut start = 0;
    let mut end = label.len();

    let mut chars_indices = label.char_indices().peekable();

    while let Some((index, char)) = chars_indices.next() {
        if !was_last_escape && char == opener {
            stack.push(index)
        }

        if !was_last_escape && char == closer {
            if let Some(start_index) = stack.pop() {
                start = start_index;
                end = index;
            }

            if stack.is_empty() && halt_condition.is_some() {
                match (chars_indices.peek(), halt_condition) {
                    (Some((_, byte)), Some(halt_condition)) if halt_condition(*byte) => {
                        break;
                    }
                    _ => {}
                }
            }
        }
        was_last_escape = char == '\\'
    }
    (start, end + 1)
}

/// Reference links are expected to be well formed:
/// [foo][bar] -> bar
/// [link \[bar][ref] -> ref
pub(super) fn find_reference_link_label(input: &str) -> &str {
    let (start, end) = find_text_within_last_set_of_balance_bracket(input, '[', ']', None);
    // +1 to move past '['
    // -1 to move before ']'
    input[start + 1..end - 1].trim()
}

/// Inline links are expected to be well formed:
/// [link](/uri) -> '/uri'
/// [link](</my uri>) -> '/my uri'
pub(super) fn find_inline_url_and_title(input: &str) -> Option<(String, Option<(String, char)>)> {
    let (_, end) =
        find_text_within_last_set_of_balance_bracket(input, '[', ']', Some(|char| char == '('));
    // +1 to move past '('
    // -1 to move before ')'
    let inline_url = input[end + 1..input.len() - 1].trim();
    if inline_url.is_empty() {
        return Some((String::new(), None));
    }

    split_inline_url_from_title(inline_url, inline_url.ends_with(['"', '\'', ')']))
}

// The link must have a title if we're calling this
fn link_title_start(link: &str) -> usize {
    let mut char_indices_rev = link.char_indices().rev().peekable();
    let (_, last) = char_indices_rev
        .next()
        .expect("links titles must have quotes");
    let opener = if last == ')' { '(' } else { last };

    while let Some((index, char)) = char_indices_rev.next() {
        if char == opener && !matches!(char_indices_rev.peek(), Some((_, '\\'))) {
            return index;
        }
    }

    // Odd case where a title is in the place of a url
    //https://spec.commonmark.org/0.30/#example-503
    0
}

/// Grab the link destination from the source text
///
/// `pulldown_cmark` unescape link destinations and titles so grabbing the escaped link
/// from the source is the easiest way to maintain all the escaped characters.
pub(super) fn recover_escaped_link_destination_and_title(
    complete_link: &str,
    link_label: &str,
    has_title: bool,
) -> Option<(String, Option<(String, char)>)> {
    let rest = reference_definition_without_label(complete_link, link_label)
        .split_once(':')
        .map(|(_, rest)| rest.trim())?;
    split_inline_url_from_title(rest, has_title)
}

/// To avoid hitting `:` within the link label.
fn reference_definition_without_label<'a>(complete_link: &'a str, link_label: &str) -> &'a str {
    // If the link label is escaped, we may not find it.
    // Then, we simply assume the link starts with the label.
    let link_label_index = complete_link.find(link_label).unwrap_or_default();
    &complete_link[(link_label_index + 1 + link_label.len())..]
}

fn trim_angle_brackes(url: &str) -> &str {
    if url.starts_with('<') && url.ends_with('>') {
        url[1..url.len() - 1].trim()
    } else {
        url.trim()
    }
}

fn split_inline_url_from_title(
    input: &str,
    has_title: bool,
) -> Option<(String, Option<(String, char)>)> {
    // If both link destination and link title are present, they must be separated by spaces, tabs,
    // and up to one line ending.
    let has_space = input.contains(char::is_whitespace);
    let only_link = !has_space && has_title;
    let link_start = link_title_start(input);
    if only_link || !has_title || link_start == 0 {
        return Some((trim_angle_brackes(input).to_string(), None));
    }

    let (mut url, mut title_with_quotes) = input.split_at(link_start);

    url = url.trim();

    title_with_quotes = title_with_quotes.trim();

    // Remove the wrapping quotes from the title
    let quote = title_with_quotes
        .chars()
        .last()
        .expect("url title has a quote");
    let title = &title_with_quotes[1..title_with_quotes.len() - 1];

    Some((
        trim_angle_brackes(url).to_string(),
        Some((title.to_string(), quote)),
    ))
}

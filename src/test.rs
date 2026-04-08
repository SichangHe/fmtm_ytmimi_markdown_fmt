#![allow(missing_docs)]

use std::{
    fs,
    path::{Path, PathBuf},
};

use insta::{Settings, assert_snapshot, glob};
use rust_search::SearchBuilder;

use super::*;

impl MarkdownFormatter<DefaultFormatterCombination> {
    pub fn from_leading_config_comments(input: &str) -> Self {
        let mut config = Config {
            max_width: None,
            ..Config::sichanghe_opinion()
        };

        let opener = "<!-- :";
        let closer = "-->";
        for l in input
            .lines()
            .take_while(|l| l.starts_with(opener) && l.ends_with(closer))
        {
            let Some((config_option, value)) = l[opener.len()..l.len() - closer.len()]
                .trim()
                .split_once(':')
            else {
                continue;
            };
            config.set(config_option, value.trim());
        }

        MarkdownFormatter::with_config(config)
    }
}

fn init_tracing() {
    _ = tracing_subscriber::fmt()
        .with_env_filter(tracing_subscriber::EnvFilter::from_default_env())
        .with_ansi(true)
        .try_init();
}

fn assert_idempotent_with_leading_config_comments(input: &str) -> String {
    let formatted = MarkdownFormatter::from_leading_config_comments(input)
        .format(input)
        .unwrap();
    let reformatted = MarkdownFormatter::from_leading_config_comments(&formatted)
        .format(&formatted)
        .unwrap();
    assert_eq!(reformatted, formatted);
    formatted
}

#[test]
fn reformat() {
    init_tracing();
    let input = r##"#  Hello World!
1.  Hey [ there! ]
2.  what's going on?

<p> and a little bit of HTML </p>

```rust
fn main() {}
```
[
    there!
    ]: htts://example.com "Yoooo"
"##;
    let mut formatter = MarkdownFormatter::default();
    formatter.sichanghe_config();
    let rewrite = formatter.format(input).unwrap();
    assert_snapshot!(rewrite)
}

#[test]
fn reformat_emoji() {
    init_tracing();
    let input = "Congratulations, that's really good news 🙂

I have a couple of good firends there.";
    let mut formatter = MarkdownFormatter::default();
    formatter.sichanghe_config();
    let rewrite = formatter.format(input).unwrap();
    assert_snapshot!(rewrite)
}

#[test]
fn reformat_display_math_in_list() {
    init_tracing();
    let input = "- $a$

    $$
    a
    $$";
    let mut formatter = MarkdownFormatter::default();
    formatter.sichanghe_config();
    let rewrite = formatter.format(input).unwrap();
    assert_snapshot!(rewrite)
}

#[test]
fn reformat_long_inline_code_in_list_is_idempotent() {
    init_tracing();
    let input = r##"<!-- : max_width: 32 -->
- `having a very very very very very very very long piece of inline code text that is longer than the max width`
"##;
    let formatted = assert_idempotent_with_leading_config_comments(input);
    assert!(
        formatted
            .lines()
            .skip(1)
            .any(|line| line.starts_with("    "))
    );
}

#[test]
fn reformat_thematic_break_followed_by_html_comment_is_idempotent() {
    init_tracing();
    let input = r##"something

---

<!-- comment that gets more blank lines before every time it gets formatted -->
"##;
    let formatted = assert_idempotent_with_leading_config_comments(input);
    assert_eq!(formatted, input);
}

#[test]
fn reformat_thematic_break_followed_by_html_block_is_idempotent() {
    init_tracing();
    let input = r##"something

---

<div>
block html under a separator
</div>
"##;
    let formatted = assert_idempotent_with_leading_config_comments(input);
    assert_eq!(formatted, input);
}

pub(crate) fn get_test_files<P: AsRef<Path>>(
    path: P,
    extension: &str,
) -> impl Iterator<Item = PathBuf> {
    SearchBuilder::default()
        .ext(extension)
        .location(path)
        .build()
        .map(PathBuf::from)
}

#[test]
fn check_markdown_formatting() {
    init_tracing();
    glob!("source/*.md", |path| {
        let input = fs::read_to_string(path).unwrap();
        let formatted_input = MarkdownFormatter::from_leading_config_comments(&input)
            .format(&input)
            .unwrap();
        let mut settings = Settings::clone_current();
        settings.set_prepend_module_to_snapshot(false);
        settings.remove_description();
        settings.remove_info();
        settings.remove_input_file();
        settings.set_snapshot_path("target/");
        settings.bind(|| {
            assert_snapshot!(formatted_input);
        });
    });
}

#[test]
fn idempotence_test() {
    init_tracing();
    glob!("target/*.snap", |path| {
        let input = fs::read_to_string(path)
            .unwrap()
            .lines()
            .skip(4)
            .collect::<Vec<_>>()
            .join("\n");
        let formatted_input = MarkdownFormatter::from_leading_config_comments(&input)
            .format(&input)
            .unwrap();
        if formatted_input != input {
            panic!(
                "Idemponency failed for `{}`:
====Original====
{input}
====Formatted====
{formatted_input}",
                path.display()
            );
        }
    });
}

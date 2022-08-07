use std::path::PathBuf;
use quote::{quote, ToTokens};
use rust_format::{Config, Edition, Formatter};

const PREETY_PLEASE_INDENT_SIZE: usize = 4;

// Unfortunately, there is no method to format just an expression but not the whole file
// but implementing the whole foematter
pub (crate) fn prettify_expression_or_variants(e: impl ToTokens) -> String {
    let file = format!("fn main() {{{}}}\n", quote!(#e).to_string());
    let config = Config::<&str, PathBuf, &str>::default()
        .option("trailing_semicolon", "false")
        .option("remove_nested_parens", "false")
        .option("newline_style", "Unix")
        .edition(Edition::Rust2021);
    let prettifier = rust_format::RustFmt::from_config(config);
    let file = prettifier.format_str(file).unwrap();
    let beggining = "fn main() {\n";
    assert!(file.starts_with(beggining));
    let ending = "\n}\n";
    assert!(file.ends_with(ending));
    let mut lines: Vec<_> = file.lines().skip(1).collect();
    lines.pop();
    let indent = " ".repeat(PREETY_PLEASE_INDENT_SIZE);
    assert!(lines.iter().all(|x| x.starts_with(&indent)));
    let lines: Vec<_> = lines.into_iter().map(|x| &x[PREETY_PLEASE_INDENT_SIZE..]).collect();
    lines.join("\n")
}

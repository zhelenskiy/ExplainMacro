extern crate core;

pub use render::StringBlock;

pub mod render;

/// Macros that logs intermediate steps of expression evaluation to the given formatter.
///
///
/// # Arguments
/// * `formatter`: `std::io::Write` or `std::fmt::Write` implementation
///
/// * `expression`: the expression for which to show intermediate states
///
/// # Returns
/// Evaluated expression
///
/// # Panics
/// The macros panics at compile-time if expression or any its part has attributes as it is not obvious how to render them.
/// It also panics at compile-time if any subexpression to be rendered does not implement `Display`.
///
/// # Examples
///
/// ```
/// use std::io::stdout;
/// use helper::explain;
/// use std::io::Write;
/// assert_eq!(explain!(stdout(), (2 + 3) * (4 + 5)), 45);
/// ```
pub use helper::explain as explain;

/// Shorthand for macros `explain` with stdout as the formatter
///
/// # Arguments:
/// * `expression`: the expression for which to show intermediate states
///
/// # Returns
/// Evaluated expression
///
/// # Panics
/// The macros panics at compile-time if expression or any its part has attributes as it is not obvious how to render them.
/// It also panics at compile-time if any subexpression to be rendered does not implement `Display`.
///
/// # Examples
///
/// ```
/// use std::io::stdout;
/// use intermediate_expr_macro::explain_stdout;
/// assert_eq!(explain_stdout!((2 + 3) * (4 + 5)), 45);
/// ```
#[macro_export] macro_rules! explain_stdout {
    ($expression:expr) => {
        {
            use std::io::Write;
            use intermediate_expr_macro::explain;
            explain!(std::io::stdout(), $expression)
        }
    };
}


/// Shorthand for macros `explain` with stderr as the formatter
///
/// # Arguments:
/// * `expression`: the expression for which to show intermediate states
///
/// # Returns
/// Evaluated expression
///
/// # Panics
/// The macros panics at compile-time if expression or any its part has attributes as it is not obvious how to render them.
/// It also panics at compile-time if any subexpression to be rendered does not implement `Display`.
///
/// # Examples
///
/// ```
/// use std::io::stderr;
/// use intermediate_expr_macro::explain_stderr;
/// assert_eq!(explain_stderr!((2 + 3) * (4 + 5)), 45);
/// ```
#[macro_export] macro_rules! explain_stderr {
    ($expression:expr) => {
        {
            use std::io::Write;
            use intermediate_expr_macro::explain;
            explain!(std::io::stderr(), $expression)
        }
    };
}


/// Shorthand for macros `explain` that accamulates output in the `String`.
/// It is helpful for testing.
///
/// # Arguments:
/// * `expression`: the expression for which to save intermediate states
///
/// # Returns
/// Unnamed tuple of accumulated log and the evaluated expression
///
/// # Panics
/// The macros panics at compile-time if expression or any its part has attributes as it is not obvious how to render them.
/// It also panics at compile-time if any subexpression to be rendered does not implement `Display`.
///
/// # Examples
///
/// ```
/// use std::io::stdout;
/// use intermediate_expr_macro::explain_string;
/// let (log, res) = explain_string!((2 + 3) * (4 + 5));
/// assert_eq!(log, "\
/// 1. 2 + 3 => 5
/// 2. 4 + 5 => 9
/// 3. (2 + 3) * (4 + 5) => 5 * 9 => 45
/// ");
/// assert_eq!(res, 45);
/// ```
#[macro_export] macro_rules! explain_string {
    ($e:expr) => {
        {
            use intermediate_expr_macro::explain;
            use std::io::Write;
            let mut v = Vec::<u8>::new();
            let res = explain!(v, $e);
            (String::from_utf8(v).unwrap(), res)
        }
    };
}

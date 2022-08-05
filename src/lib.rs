extern crate core;

pub use render::StringBlock;

pub mod render;

pub use helper::explain as explain;

#[macro_export] macro_rules! explain_stdout {
    ($e:expr) => {
        {
            use std::io::Write;
            use intermediate_expr_macro::explain;
            explain!(std::io::stdout(), $e)
        }
    };
}

#[macro_export] macro_rules! explain_stderr {
    ($e:expr) => {
        {
            use std::io::Write;
            use intermediate_expr_macro::explain;
            explain!(std::io::stderr(), $e)
        }
    };
}

#[macro_export] macro_rules! explain_str {
    ($e:expr) => {
        {
            use intermediate_expr_macro::explain;
            let mut v = Vec::<u8>::new();
            let res = explain!(v, $e);
            (String::from_utf8(v).unwrap(), res)
        }
    };
}

#[cfg(test)]
mod tests {

    #[test]
    fn it_works() {
        let result = 2 + 2;
        assert_eq!(result, 4);
    }
}

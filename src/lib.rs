extern crate core;

pub use render::StringBlock;

pub mod render;

pub use helper::explain as explain;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        let result = 2 + 2;
        assert_eq!(result, 4);
    }
}

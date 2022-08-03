use proc_macro::TokenStream;
use std::cmp::max;
use quote::quote;
use syn::{Attribute, Expr, ExprArray, ExprAssign};
use crate::render::{Alignment, join, StringBlock};

pub trait ExplarationNode {
    fn eval(self) -> TokenStream;
}

pub fn combine_source_and_result(source: StringBlock, result: StringBlock) -> StringBlock {
    let width = max(source.width(), result.width());
    let separator = String::from_iter((0..width).map(|_| '_'));
    source
        .add_to_bottom(StringBlock::from_str(&separator), Alignment::Center)
        .add_to_bottom(result, Alignment::Center)
}

impl ExplarationNode for Expr {
    fn eval(self) -> TokenStream {
        todo!()
    }
}

// impl ExplarationNode for ExprArray {
//     fn eval(self) -> TokenStream {
//         self.
//         let x = quote! {
//             [#self]
//         };
//         x.into()
//     }
// }
//
// impl ExplarationNode for ExprAssign {
//     fn render_source(&self) -> StringBlock {
//         handle_attributes(&self.attrs);
//         self.left.render_all() + " = " + self.right.render_all()
//     }
//
//     fn render_result(&self) -> Option<String> { None }
// }

fn handle_attributes(attrs: &Vec<Attribute>) {
    assert!(attrs.is_empty(), "Attributes are not supported");
}

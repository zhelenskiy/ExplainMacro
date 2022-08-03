use proc_macro::TokenStream;
use quote::quote;
use syn::{Expr, parse_macro_input};

mod node;
mod render;


// fn arguments(e: Expr) -> Vec<Expr> {
//     match e {
        // Expr::Array(arr) => arr.elems.into_iter().collect(),
        // Expr::Assign(op) => vec![*op.left, *op.right],
        // Expr::AssignOp(op) => vec![*op.left, *op.right],
        // Expr::Async(_) => vec![],
        // Expr::Await(expr) => vec![*expr.base],
        // Expr::Binary(op) => vec![*op.left, *op.right],
        // Expr::Block(_) => {}
        // Expr::Box(_) => {}
        // Expr::Break(_) => {}
        // Expr::Call(_) => {}
        // Expr::Cast(_) => {}
        // Expr::Closure(_) => {}
        // Expr::Continue(_) => {}
        // Expr::Field(_) => {}
        // Expr::ForLoop(_) => {}
        // Expr::Group(_) => {}
        // Expr::If(_) => {}
        // Expr::Index(_) => {}
        // Expr::Let(_) => {}
        // Expr::Lit(_) => {}
        // Expr::Loop(_) => {}
        // Expr::Macro(_) => {}
        // Expr::Match(_) => {}
        // Expr::MethodCall(_) => {}
        // Expr::Paren(_) => {}
        // Expr::Path(_) => {}
        // Expr::Range(_) => {}
        // Expr::Reference(_) => {}
        // Expr::Repeat(_) => {}
        // Expr::Return(_) => {}
        // Expr::Struct(_) => {}
        // Expr::Try(_) => {}
        // Expr::TryBlock(_) => {}
        // Expr::Tuple(_) => {}
        // Expr::Type(_) => {}
        // Expr::Unary(_) => {}
        // Expr::Unsafe(_) => {}
        // Expr::Verbatim(_) => {}
        // Expr::While(_) => {}
        // Expr::Yield(_) => {}
        // Expr::__NonExhaustive => {}
    // }
// }

#[proc_macro]
pub fn explain(item: TokenStream) -> TokenStream {
    let old = item.clone();
    let expr: Expr = parse_macro_input!(item as Expr);
    match expr {
        Expr::Array(_) => {}
        Expr::Assign(_) => {}
        Expr::AssignOp(_) => {}
        Expr::Async(_) => {}
        Expr::Await(_) => {}
        Expr::Binary(_) => {}
        Expr::Block(_) => {}
        Expr::Box(_) => {}
        Expr::Break(_) => {}
        Expr::Call(_) => {}
        Expr::Cast(_) => {}
        Expr::Closure(_) => {}
        Expr::Continue(_) => {}
        Expr::Field(_) => {}
        Expr::ForLoop(_) => {}
        Expr::Group(_) => {}
        Expr::If(_) => {}
        Expr::Index(_) => {}
        Expr::Let(_) => {}
        Expr::Lit(v) => {
            let code = quote! { { println!("Heh: {}", #v); #v } };
            return code.into()
        }
        Expr::Loop(_) => {}
        Expr::Macro(_) => {}
        Expr::Match(_) => {}
        Expr::MethodCall(_) => {}
        Expr::Paren(_) => {}
        Expr::Path(_) => {}
        Expr::Range(_) => {}
        Expr::Reference(_) => {}
        Expr::Repeat(_) => {}
        Expr::Return(_) => {}
        Expr::Struct(_) => {}
        Expr::Try(_) => {}
        Expr::TryBlock(_) => {}
        Expr::Tuple(_) => {}
        Expr::Type(_) => {}
        Expr::Unary(_) => {}
        Expr::Unsafe(_) => {}
        Expr::Verbatim(_) => {}
        Expr::While(_) => {}
        Expr::Yield(_) => {}
        _ => {}
    }
    // println!("{:#?}", ast);
    old
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        let result = 2 + 2;
        assert_eq!(result, 4);
    }
}

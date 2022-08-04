extern crate core;

use proc_macro::{TokenStream};
use quote::quote;
use syn::{Expr, parse_macro_input, Attribute, UnOp, ExprAssignOp, ExprBinary, ExprIndex, ExprCast, ExprField, ExprUnary, ExprCall, ExprMethodCall, ExprParen, Ident};
use syn::__private::{Span, TokenStream2};
use crate::expression_transformer::{defaults, ExprTransformer, transform_box_expr};

mod render;
mod expression_transformer;

#[proc_macro]
pub fn explain(item: TokenStream) -> TokenStream {
    let expr: Expr = parse_macro_input!(item as Expr);
    let mut tranformer = CodeTransformer { indent: 0, index: 1 };
    let x = tranformer.transform_expr(expr);
    quote!(#x).into()
}

fn handle_attributes(attrs: &Vec<Attribute>) {
    assert!(attrs.is_empty(), "Attributes are not supported");
}

struct CodeTransformer {
    pub index: u32,
    pub indent: u32, // todo
}

fn quote_to_expr(tokens: TokenStream2) -> Expr {
    syn::parse(tokens.into()).unwrap()
}

fn explain_zero_arg(transformer: &mut CodeTransformer, expr: Expr) -> Expr {
    let string = quote!(#expr).to_string();
    let expr = crate::expression_transformer::defaults::transform_children(transformer, expr);
    let cur_index = transformer.index.clone();
    transformer.index += 1;
    quote_to_expr(quote! {
        {
            let x = #expr;
            print!("{}. {} => ", #cur_index, #string);
            println!("{}", x);
            x
        }
    })
}

impl ExprTransformer for CodeTransformer {
    fn transform_assign_op(&mut self, assign: ExprAssignOp) -> Expr {
        if !needs_intermediate_display(&assign.left) && !needs_intermediate_display(&assign.right) {
            return defaults::transform_assign_op(self, assign)
        }
        handle_attributes(&assign.attrs);
        let string = quote!(#assign).to_string();
        let name = {
            let x = &assign.left;
            quote!(#x).to_string()
        };
        let left = transform_box_expr(self, assign.left);
        let right = transform_box_expr(self, assign.right);
        let op_symbol = assign.op;
        let op_symbol_str = quote!(#op_symbol).to_string();
        let cur_index = self.index.clone();
        self.index += 1;
        return quote_to_expr(quote! {
            {
                let mut left = #left ;
                let right = #right ;
                print!("{}. ({}) => ", #cur_index, #string) ;
                print!("({}/* = {} */ {} {}) => ", #name, left, #op_symbol_str, right) ;
                let res = left #op_symbol right ;
                println!( "{}", left) ;
                res
            }
        });
    }

    fn transform_binary(&mut self, binary: ExprBinary) -> Expr {
        if !needs_intermediate_display(&binary.left) && !needs_intermediate_display(&binary.right) {
            return explain_zero_arg(self, Expr::Binary(binary));
        }
        handle_attributes(&binary.attrs);
        let string = quote!( #binary ).to_string();
        let left = transform_box_expr(self, binary.left);
        let right = transform_box_expr(self, binary.right);
        let op_symbol = binary.op;
        let op_symbol_str = quote!( #op_symbol ).to_string();
        let cur_index = self.index.clone();
        self.index += 1;
        return quote_to_expr(quote! {
            {
                let left = #left ;
                let right = #right ;
                print!( "{}. {} => ", #cur_index, #string) ;
                print!( "{} {} {} => ", left, #op_symbol_str, right) ;
                let res = left #op_symbol right ;
                println!( "{}", res);
                res
            }
        });
    }

    fn transform_index(&mut self, index: ExprIndex) -> Expr {
        if !needs_intermediate_display(&index.expr) && !needs_intermediate_display(&index.index) {
            return explain_zero_arg(self, Expr::Index(index));
        }
        handle_attributes(&index.attrs);
        let string = quote!(#index).to_string();
        let collection_str = {
            let x = &*index.expr;
            quote!(#x).to_string()
        };
        let expr = transform_box_expr(self, index.expr);
        let index = transform_box_expr(self, index.index);
        let cur_index = self.index.clone();
        self.index += 1;
        return quote_to_expr(quote! {
            {
                let expr = #expr;
                let index = #index;
                print!("{}. {} => ", #cur_index, #string);
                print!("{}[{}] => ", #collection_str, index);
                let res = expr[index];
                println!("{}", res);
                res
            }
        });
    }

    fn transform_cast(&mut self, cast: ExprCast) -> Expr {
        if !needs_intermediate_display(&cast.expr) {
            return explain_zero_arg(self, Expr::Cast(cast));
        }
        handle_attributes(&cast.attrs);
        let string = quote!(#cast).to_string();
        let expr = transform_box_expr(self, cast.expr);
        let cur_index = self.index.clone();
        self.index += 1;
        let type_ = *cast.ty;
        let type_str = quote!(#type_).to_string();
        return quote_to_expr(quote! {
            {
                let expr = #expr;
                print!("{}. {} => ", #cur_index, #string);
                print!("{} as {} => ", expr, #type_str);
                let res = expr as #type_;
                println!("{}", res);
                res
            }
        });
    }

    fn transform_field(&mut self, field: ExprField) -> Expr {
        if !needs_intermediate_display(&field.base) {
            return explain_zero_arg(self, Expr::Field(field));
        }
        handle_attributes(&field.attrs);
        let string = quote!(#field).to_string();
        let expr = transform_box_expr(self, field.base);
        let cur_index = self.index.clone();
        self.index += 1;
        let member = field.member;
        let member_str = quote!(#member).to_string();
        return quote_to_expr(quote! {
            {
                let expr = #expr;
                print!("{}. {} => ", #cur_index, #string);
                print!("{}.{} => ", expr, #member_str);
                let res = expr.#member;
                println!("{}", res);
                res
            }
        });
    }

    fn transform_unary(&mut self, unary: ExprUnary) -> Expr {
        if !needs_intermediate_display(&unary.expr) {
            return explain_zero_arg(self, Expr::Unary(unary));
        }
        handle_attributes(&unary.attrs);
        let string = quote!(#unary).to_string();
        let expr = transform_box_expr(self, unary.expr);
        let cur_index = self.index.clone();
        self.index += 1;
        let op_symbol = unary.op;
        let op_symbol_str = quote!(#op_symbol).to_string();
        return quote_to_expr(quote! {
            {
                let expr = #expr;
                print!("{}. {} => ", #cur_index, #string);
                print!("{}({}) => ", #op_symbol_str, expr);
                let res = #op_symbol expr;
                println!("{}", res);
                res
            }
        });
    }

    fn transform_call(&mut self, call: ExprCall) -> Expr {
        // do not use transform call.func as it is expected not to be implementing trait Display
        if call.args.iter().all(|x| !needs_intermediate_display(&x)) {
            return explain_zero_arg(self, Expr::Call(call));
        }
        handle_attributes(&call.attrs);
        let string = quote!(#call).to_string();
        let old_args: Vec<_> = call.args.into_iter().map(|x| self.transform_expr(x)).collect();
        let cur_index = self.index.clone();
        self.index += 1;
        let function = *call.func;
        let function_str = quote!(#function).to_string();

        let let_expressions: Vec<_> = old_args.into_iter()
            .enumerate()
            .map(|(index, expr)| {
                let name = format!("arg{}", index);
                let name = Ident::new(name.as_str(), Span::call_site());
                quote_to_expr(quote! {let #name = #expr })
            })
            .collect();

        let new_args: Vec<_> = let_expressions.iter()
            .enumerate()
            .map(|(index, _)| {
                let name = format!("arg{}", index);
                let name = Ident::new(name.as_str(), Span::call_site());
                quote_to_expr(quote!(#name))
            })
            .collect();

        let parens: Vec<&str> = let_expressions.iter().map(|_| "{}").collect();
        let pattern = String::from("{}(") + &parens.join("") + &String::from(") => ");

        return quote_to_expr(quote! {
            {
                #(#let_expressions);*;
                print!("{}. {} => ", #cur_index, #string);
                print!(#pattern, #function_str, #(#new_args),*);
                let res = #function(#(#new_args),*);
                println!("{}", res);
                res
            }
        });
    }

    fn transform_method_call(&mut self, method_call: ExprMethodCall) -> Expr {
        if method_call.args.iter().all(|x| !needs_intermediate_display(&x)) {
            return explain_zero_arg(self, Expr::MethodCall(method_call));
        }
        handle_attributes(&method_call.attrs);
        let string = quote!(#method_call).to_string();
        let old_args: Vec<_> = method_call.args.into_iter().map(|x| self.transform_expr(x)).collect();
        let cur_index = self.index.clone();
        self.index += 1;
        let function = method_call.method;
        let function_str = quote!(#function).to_string();

        let receiver = self.transform_expr(*method_call.receiver);

        let turbofish = method_call.turbofish;
        let turbofish_str = turbofish.clone().map_or(String::new(), |x| quote!(#x).to_string());

        let let_expressions: Vec<_> = old_args.into_iter()
            .enumerate()
            .map(|(index, expr)| {
                let name = format!("arg{}", index);
                let name = Ident::new(name.as_str(), Span::call_site());
                quote_to_expr(quote!(let #name = #expr;))
            })
            .collect();

        let new_args: Vec<_> = let_expressions.iter()
            .enumerate()
            .map(|(index, _)| {
                let name = format!("arg{}", index);
                let name = Ident::new(name.as_str(), Span::call_site());
                quote_to_expr(quote!(#name))
            })
            .collect();

        let parens: Vec<&str> = let_expressions.iter().map(|_| "{}").collect();
        let pattern = String::from("{}.{}{}(") + &parens.join("") + &String::from(") => ");

        return quote_to_expr(quote! {
            {
                let receiver = #receiver;
                #(#let_expressions)*
                print!("{}. {} => ", #cur_index, #string);
                print!(#pattern, receiver, #turbofish_str, #function_str, #(#new_args),*);
                let res = receiver.#function #turbofish(#(#new_args),*);
                println!("{}", res);
                res
            }
        });
    }

    fn transform_parenthesis(&mut self, parenthesis: ExprParen) -> Expr {
        let expr = transform_box_expr(self, parenthesis.expr);
        if let Expr::Block(_) = *expr {
            return *expr;
        }
        Expr::Paren(ExprParen {
            expr: expr,
            ..parenthesis
        })
    }
}

fn needs_intermediate_display(expression: &Expr) -> bool {
    match &expression {
        Expr::AssignOp(_) | Expr::Binary(_) | Expr::Call(_) | Expr::Cast(_) | Expr::Field(_) | Expr::Index(_) | Expr::MethodCall(_) => true,
        Expr::Unary(u) => match u.op {
            UnOp::Deref(_) => false,
            UnOp::Neg(_) | UnOp::Not(_) => true
        }
        Expr::Paren(p) => needs_intermediate_display(&p.expr),
        Expr::Group(g) => needs_intermediate_display(&g.expr),
        _ => false
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        let result = 2 + 2;
        assert_eq!(result, 4);
    }
}

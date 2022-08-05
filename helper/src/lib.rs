use proc_macro::TokenStream;
use quote::quote;
use syn::{Attribute, Expr, ExprAssignOp, ExprBinary, ExprBlock, ExprCall, ExprCast, ExprField, ExprForLoop, ExprIf, ExprIndex, ExprLoop, ExprMatch, ExprMethodCall, ExprParen, ExprTryBlock, ExprUnary, ExprUnsafe, ExprWhile, Ident, UnOp};
use syn::__private::{Span, TokenStream2};
use crate::expression_transformer::{defaults, ExprTransformer, transform_box_expr};

mod expression_transformer;

fn handle_attributes(attrs: &Vec<Attribute>) {
    assert!(attrs.is_empty(), "Attributes are not supported");
}

#[proc_macro]
pub fn explain(item: TokenStream) -> TokenStream {
    let expr: Expr = syn::parse_macro_input!(item as Expr);
    let mut tranformer = CodeTransformer { indent: 0, index: 1 };
    let x = tranformer.transform_expr(expr);
    quote!(#x).into()
}


struct CodeTransformer {
    pub index: u32,
    pub indent: usize,
}

fn quote_to_expr(tokens: TokenStream2) -> Expr {
    syn::parse(tokens.into()).unwrap()
}

const TAB_SIZE: usize = 4;

fn explain_zero_arg(transformer: &mut CodeTransformer, expr: Expr) -> Expr {
    let string = quote!(#expr).to_string();
    let expr = crate::expression_transformer::defaults::transform_children(transformer, expr);
    let cur_index = transformer.index.clone();
    transformer.index += 1;
    let indent = transformer.indent * TAB_SIZE;
    quote_to_expr(quote! {
        {
            let x = #expr;
            let sb = intermediate_expr_macro::render::StringBlock::new()
                + " ".repeat(#indent)
                + format!("{}. ", #cur_index)
                + format!("{}", #string)
                + " => "
                + format!("{}", x);
            println!("{}", sb);
            x
        }
    })
}

impl ExprTransformer for CodeTransformer {
    fn transform_assign_op(&mut self, assign: ExprAssignOp) -> Expr {
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
        let indent = self.indent * TAB_SIZE;
        return quote_to_expr(quote! {
            {
                let left = &mut #left;
                let right = #right;
                let sb = intermediate_expr_macro::render::StringBlock::new()
                    + " ".repeat(#indent)
                    + format!("{}. (", #cur_index)
                    + format!("{}", #string)
                    + format!(") => ({}/* = ", #name)
                    + format!("{} */", left)
                    + format!(" {} ", #op_symbol_str)
                    + format!("{}", right)
                    + ") => ";
                let res = *left #op_symbol right;
                println!("{}", sb + format!("{}", left));
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
        let indent = self.indent * TAB_SIZE;
        return quote_to_expr(quote! {
            {
                let left = #left;
                let right = #right;
                let sb = intermediate_expr_macro::render::StringBlock::new()
                    + " ".repeat(#indent)
                    + format!("{}. ", #cur_index)
                    + format!("{}", #string)
                    + " => "
                    + format!("{}", left)
                    + format!(" {} ", #op_symbol_str)
                    + format!("{}", right)
                    + " => ";
                let res = left #op_symbol right;
                println!("{}", sb + format!("{}", res));
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
        let indent = self.indent * TAB_SIZE;
        return quote_to_expr(quote! {
            {
                let expr = #expr;
                let index = #index;
                let sb = intermediate_expr_macro::render::StringBlock::new()
                    + " ".repeat(#indent)
                    + format!("{}. ", #cur_index)
                    + format!("{}", #string)
                    + format!(" => {}[", #collection_str)
                    + format!("{}", index)
                    + "] => ";
                let res = expr[index];
                println!("{}", sb + format!("{}", res));
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
        let indent = self.indent * TAB_SIZE;
        return quote_to_expr(quote! {
            {
                let expr = #expr;
                let sb = intermediate_expr_macro::render::StringBlock::new()
                    + " ".repeat(#indent)
                    + format!("{}. ", #cur_index)
                    + format!("{}", #string)
                    + " => "
                    + format!("{}", expr)
                    + format!(" as {} => ", #type_str);
                let res = expr as #type_;
                println!("{}", sb + format!("{}", res));
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
        let indent = self.indent * TAB_SIZE;
        return quote_to_expr(quote! {
            {
                let expr = #expr;
                let sb = intermediate_expr_macro::render::StringBlock::new()
                    + " ".repeat(#indent)
                    + format!("{}. ", #cur_index)
                    + format!("{}", #string)
                    + " => "
                    + format!("{}", expr)
                    + format!(".{} => ", #member_str);
                let res = expr.#member;
                println!("{}", sb + format!("{}", res));
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
        let indent = self.indent * TAB_SIZE;
        return quote_to_expr(quote! {
            {
                let expr = #expr;
                let sb = intermediate_expr_macro::render::StringBlock::new()
                    + " ".repeat(#indent)
                    + format!("{}. ", #cur_index)
                    + format!("{}", #string)
                    + " => "
                    + format!("{}(", #op_symbol_str)
                    + format!("{}", expr)
                    + ") => ";
                let res = #op_symbol expr;
                println!("{}", sb + format!("{}", res));
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

        let (first, rest) = {
            let mut iter = new_args.iter();
            (iter.next().into_iter(), iter)
        };

        let indent = self.indent * TAB_SIZE;
        return quote_to_expr(quote! {
            {
                #(#let_expressions);*;
                let sb = intermediate_expr_macro::render::StringBlock::new()
                    + " ".repeat(#indent)
                    + format!("{}. ", #cur_index)
                    + format!("{}", #string)
                    + " => "
                    + format!("{}", #function_str)
                    + "("
                    + #(format!("{}", #first))+* #(+ ", " + format!("{}", #rest))*
                    + ") => ";
                let res = #function(#(#new_args),*);
                println!("{}", sb + format!("{}", res));
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
                quote_to_expr(quote!(let #name = #expr))
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

        let (first, rest) = {
            let mut iter = new_args.iter();
            (iter.next().into_iter(), iter)
        };

        let indent = self.indent * TAB_SIZE;
        return quote_to_expr(quote! {
            {
                let receiver = #receiver;
                #(#let_expressions);*;
                let sb = intermediate_expr_macro::render::StringBlock::new()
                    + " ".repeat(#indent)
                    + format!("{}. ", #cur_index)
                    + format!("{}", #string)
                    + " => "
                    + format!("{}", receiver)
                    + format!(".{}{}(", #function_str, #turbofish_str)
                    + #(format!("{}", #first))+* #(+ ", " + format!("{}", #rest))*
                    + ") => ";
                let res = receiver.#function #turbofish(#(#new_args),*);
                println!("{}", sb + format!("{}", res));
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

    fn transform_while(&mut self, while_expr: ExprWhile) -> Expr {
        let cond = &*while_expr.cond;
        self.named_block(&format!("while {}", quote!(#cond).to_string()), Expr::While(while_expr))
    }

    fn transform_for(&mut self, exp_for: ExprForLoop) -> Expr {
        let pattern = &exp_for.pat;
        let expr = &*exp_for.expr;
        self.named_block(&format!("for {} in {}", quote!(#pattern).to_string(), quote!(#expr).to_string()), Expr::ForLoop(exp_for))
    }

    fn transform_loop(&mut self, exp_loop: ExprLoop) -> Expr {
        self.named_block("loop", Expr::Loop(exp_loop))
    }

    fn transform_block_expr(&mut self, block: ExprBlock) -> Expr {
        self.named_block("", Expr::Block(block))
    }

    fn transform_unsafe(&mut self, unsafe_expr: ExprUnsafe) -> Expr {
        self.named_block("unsafe", Expr::Unsafe(unsafe_expr))
    }

    fn transform_try_block(&mut self, try_block: ExprTryBlock) -> Expr {
        self.named_block("try", Expr::TryBlock(try_block))
    }

    fn transform_if(&mut self, if_expr: ExprIf) -> Expr {
        let indent = self.indent * TAB_SIZE;
        self.indent += 1;
        let next_indent = self.indent * TAB_SIZE;
        let cond_str = format!("if {}", { let c = &*if_expr.cond; quote!(#c).to_string() });
        let cond = self.transform_expr(*if_expr.cond);
        let then_branch = self.transform_block(if_expr.then_branch);
        let else_branch = if_expr.else_branch.map(|x|
            match *x.1 {
                Expr::Block(b) => defaults::transform_block_expr(self, b),
                x => self.transform_expr(x)
            }
        );
        self.indent -= 1;
        quote_to_expr(quote! {
            {
                println!("{}{} {{", " ".repeat(#indent), #cond_str);
                let cond = #cond;
                let t = if cond {
                    println!("{}then:", " ".repeat(#next_indent));
                    #then_branch
                } else {
                    println!("{}else:", " ".repeat(#next_indent));
                    #else_branch
                };
                println!("{}}}", " ".repeat(#indent));
                t
            }
        })
    }

    fn transform_match(&mut self, match_expr: ExprMatch) -> Expr {
        let expr = &*match_expr.expr;
        self.named_block(&format!("match {}", quote!(#expr).to_string()), Expr::Match(match_expr))
    }
}

impl CodeTransformer {
    fn named_block(&mut self, name: &str, expr: Expr) -> Expr {
        let indent = self.indent * TAB_SIZE;
        self.indent += 1;
        let transformed = defaults::transform_children(self, expr);
        self.indent -= 1;
        let name_space = if name.is_empty() { String::from(name) } else { format!("{name} ") };
        quote_to_expr(quote! {
            {
                println!("{}{}{{", " ".repeat(#indent), #name_space);
                let t = #transformed;
                println!("{}}}", " ".repeat(#indent));
                t
            }
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

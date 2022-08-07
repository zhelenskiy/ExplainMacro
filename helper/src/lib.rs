use proc_macro::TokenStream;
use quote::quote;
use syn::{Arm, Attribute, BinOp, Block, Expr, ExprAssignOp, ExprBinary, ExprBlock, ExprCall, ExprCast, ExprField, ExprForLoop, ExprIf, ExprIndex, ExprLit, ExprLoop, ExprMatch, ExprMethodCall, ExprParen, ExprTryBlock, ExprUnary, ExprUnsafe, ExprWhile, GenericMethodArgument, Ident, Lit, Pat, Stmt, Type, UnOp};
use syn::__private::{Span, TokenStream2};
use syn::parse::Parser;
use syn::token::Semi;
use crate::expression_transformer::{defaults, ExprTransformer, transform_box_expr};
use crate::pretty_printer::{prettify_expression_or_variants};

mod expression_transformer;
mod pretty_printer;

fn handle_attributes(attrs: &Vec<Attribute>) {
    assert!(attrs.is_empty(), "Attributes are not supported");
}

#[proc_macro]
pub fn explain(item: TokenStream) -> TokenStream {
    let args_parsed = syn::punctuated::Punctuated::<syn::Expr, syn::Token![,]>::parse_terminated
        .parse(item)
        .unwrap();
    if args_parsed.len() != 2 {
        panic!("Explain macros takes formatter and expression");
    }
    let (formatter, expr) = {
        let mut it = args_parsed.into_iter();
        (it.next().unwrap(), it.next().unwrap())
    };
    let mut tranformer = CodeTransformer { formatter, indent: 0, index: 1 };
    let x = tranformer.transform_expr(expr);
    quote!(#x).into()
}


struct CodeTransformer {
    pub formatter: Expr,
    pub index: u32,
    pub indent: usize,
}

fn quote_to_expr(tokens: TokenStream2) -> Expr {
    syn::parse(tokens.into()).unwrap()
}

const TAB_SIZE: usize = 4;

fn explain_zero_arg(transformer: &mut CodeTransformer, expr: Expr) -> Expr {
    let string = prettify_expression_or_variants(&expr);
    let expr = crate::expression_transformer::defaults::transform_children(transformer, expr);
    let cur_index = transformer.index.clone();
    transformer.index += 1;
    let indent = transformer.indent * TAB_SIZE;
    let formatter = &transformer.formatter;
    quote_to_expr(quote! {
        {
            let x = #expr;
            let sb = intermediate_expr_macro::render::StringBlock::new()
                + " ".repeat(#indent)
                + format!("{}. ", #cur_index)
                + format!("{}", #string)
                + " => "
                + format!("{}", x);
            writeln!(#formatter, "{}", sb).unwrap();
            x
        }
    })
}

impl ExprTransformer for CodeTransformer {
    fn transform_assign_op(&mut self, assign: ExprAssignOp) -> Expr {
        handle_attributes(&assign.attrs);
        let string = prettify_expression_or_variants(&assign);
        let name = prettify_expression_or_variants(&assign.left);
        let left = transform_box_expr(self, assign.left);
        let right = transform_box_expr(self, assign.right);
        let op_symbol = assign.op;
        let op_symbol_str = quote!(#op_symbol).to_string();
        let cur_index = self.index.clone();
        self.index += 1;
        let indent = self.indent * TAB_SIZE;
        let formatter = &self.formatter;
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
                writeln!(#formatter, "{}", sb + format!("{}", left)).unwrap();
                res
            }
        });
    }

    fn transform_binary(&mut self, binary: ExprBinary) -> Expr {
        if !needs_intermediate_display(&binary.left) && !needs_intermediate_display(&binary.right) {
            return explain_zero_arg(self, Expr::Binary(binary));
        }
        handle_attributes(&binary.attrs);
        let string = prettify_expression_or_variants(&binary);
        let left = transform_box_expr(self, binary.left);
        let right = transform_box_expr(self, binary.right);
        let op_symbol = binary.op;
        let op_symbol_str = quote!( #op_symbol ).to_string();
        let cur_index = self.index.clone();
        self.index += 1;
        let indent = self.indent * TAB_SIZE;
        let formatter = &self.formatter;
        return match op_symbol {
            BinOp::And(_) => quote_to_expr(quote! {
                {
                    let left = #left;
                    let (sb, res) = if left {
                        let right = #right;
                        let sb = intermediate_expr_macro::render::StringBlock::new()
                            + " ".repeat(#indent)
                            + format!("{}. ", #cur_index)
                            + format!("{}", #string)
                            + " => true && "
                            + format!("{}", right)
                            + " => ";
                        (sb, right)
                    } else {
                        let sb = intermediate_expr_macro::render::StringBlock::new()
                            + " ".repeat(#indent)
                            + format!("{}. ", #cur_index)
                            + format!("{}", #string)
                            + " => false && ... => ";
                        (sb, false)
                    };
                    writeln!(#formatter, "{}", sb + format!("{}", res)).unwrap();
                    res
                }
            }),
            BinOp::Or(_) => quote_to_expr(quote! {
                {
                    let left = #left;
                    let (sb, res) = if left {
                        let sb = intermediate_expr_macro::render::StringBlock::new()
                            + " ".repeat(#indent)
                            + format!("{}. ", #cur_index)
                            + format!("{}", #string)
                            + " => true || ... => ";
                        (sb, true)
                    } else {
                        let right = #right;
                        let sb = intermediate_expr_macro::render::StringBlock::new()
                            + " ".repeat(#indent)
                            + format!("{}. ", #cur_index)
                            + format!("{}", #string)
                            + " => false || "
                            + format!("{}", right)
                            + " => ";
                        (sb, right)
                    };
                    writeln!(#formatter, "{}", sb + format!("{}", res)).unwrap();
                    res
                }
            }),
            _ => quote_to_expr(quote! {
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
                    writeln!(#formatter, "{}", sb + format!("{}", res)).unwrap();
                    res
                }
            })
        };
    }

    fn transform_index(&mut self, index: ExprIndex) -> Expr {
        if !needs_intermediate_display(&index.expr) && !needs_intermediate_display(&index.index) {
            return explain_zero_arg(self, Expr::Index(index));
        }
        handle_attributes(&index.attrs);
        let string = prettify_expression_or_variants(&index);
        let collection_str = prettify_expression_or_variants(&*index.expr);
        let expr = transform_box_expr(self, index.expr);
        let index = transform_box_expr(self, index.index);
        let cur_index = self.index.clone();
        self.index += 1;
        let indent = self.indent * TAB_SIZE;
        let formatter = &self.formatter;
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
                writeln!(#formatter, "{}", sb + format!("{}", res)).unwrap();
                res
            }
        });
    }

    fn transform_cast(&mut self, cast: ExprCast) -> Expr {
        if !needs_intermediate_display(&cast.expr) {
            return explain_zero_arg(self, Expr::Cast(cast));
        }
        handle_attributes(&cast.attrs);
        let string = prettify_expression_or_variants(&cast);
        let expr = transform_box_expr(self, cast.expr);
        let cur_index = self.index.clone();
        self.index += 1;
        let type_ = *cast.ty;
        let type_str = prettify_type(&type_);
        let indent = self.indent * TAB_SIZE;
        let formatter = &self.formatter;
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
                writeln!(#formatter, "{}", sb + format!("{}", res)).unwrap();
                res
            }
        });
    }

    fn transform_field(&mut self, field: ExprField) -> Expr {
        if !needs_intermediate_display(&field.base) {
            return explain_zero_arg(self, Expr::Field(field));
        }
        handle_attributes(&field.attrs);
        let string = prettify_expression_or_variants(&field);
        let expr = transform_box_expr(self, field.base);
        let cur_index = self.index.clone();
        self.index += 1;
        let member = field.member;
        let member_str = quote!(#member).to_string();
        let indent = self.indent * TAB_SIZE;
        let formatter = &self.formatter;
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
                writeln!(#formatter, "{}", sb + format!("{}", res)).unwrap();
                res
            }
        });
    }

    fn transform_unary(&mut self, unary: ExprUnary) -> Expr {
        if let ExprUnary { op: UnOp::Neg(_), expr, .. } = &unary {
            if let Expr::Lit(ExprLit { lit, .. }) = &**expr {
                match lit {
                    Lit::Float(_) | Lit::Int(_) => return Expr::Unary(unary),
                    _ => ()
                }
            }
        } else if let ExprUnary { op: UnOp::Deref(_), .. } = &unary {
            // 1. dereferencing is usually meaningless operation
            // 2. pointers do not usually implement trait Display
            // 3. It causes difficalties with necessety of mutability modifier
            return Expr::Unary(unary);
        }

        if !needs_intermediate_display(&unary.expr) {
            return explain_zero_arg(self, Expr::Unary(unary));
        }

        handle_attributes(&unary.attrs);
        let string = prettify_expression_or_variants(&unary);
        let expr = transform_box_expr(self, unary.expr);
        let cur_index = self.index.clone();
        self.index += 1;
        let op_symbol = unary.op;
        let op_symbol_str = quote!(#op_symbol).to_string();
        let indent = self.indent * TAB_SIZE;
        let formatter = &self.formatter;
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
                writeln!(#formatter, "{}", sb + format!("{}", res)).unwrap();
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
        let string = prettify_expression_or_variants(&call);
        let old_args: Vec<_> = call.args.into_iter().map(|x| self.transform_expr(x)).collect();
        let cur_index = self.index.clone();
        self.index += 1;
        let function = *call.func;
        let function_str = prettify_function(&function);

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
        let formatter = &self.formatter;
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
                    #(+ format!("{}", #first))+* #(+ ", " + format!("{}", #rest))*
                    + ") => ";
                let res = #function(#(#new_args),*);
                writeln!(#formatter, "{}", sb + format!("{}", res)).unwrap();
                res
            }
        });
    }

    fn transform_method_call(&mut self, method_call: ExprMethodCall) -> Expr {
        if method_call.args.iter().all(|x| !needs_intermediate_display(&x)) && !needs_intermediate_display(&method_call.receiver) {
            return explain_zero_arg(self, Expr::MethodCall(method_call));
        }
        handle_attributes(&method_call.attrs);
        let string = prettify_expression_or_variants(&method_call);
        let receiver = self.transform_expr(*method_call.receiver);
        let old_args: Vec<_> = method_call.args.into_iter().map(|x| self.transform_expr(x)).collect();
        let cur_index = self.index.clone();
        self.index += 1;
        let function = method_call.method;
        let turbofish = method_call.turbofish;
        let function_str = quote!(#function).to_string();


        let turbofish_str = turbofish.clone().map_or(String::new(), |x| {
            let args: Vec<_> = x.args.iter().map(|x| match x {
                GenericMethodArgument::Type(t) => prettify_type(t),
                GenericMethodArgument::Const(e) => prettify_expression_or_variants(e)
            }).collect();
            format!("::<{}>", args.join(", "))
        });

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
        let formatter = &self.formatter;
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
                    #(+ format!("{}", #first))+* #(+ ", " + format!("{}", #rest))*
                    + ") => ";
                let res = receiver . #function #turbofish (#(#new_args),*);
                writeln!(#formatter, "{}", sb + format!("{}", res)).unwrap();
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
        self.named_block(&format!("while {}", prettify_expression_or_variants(cond)), Expr::While(while_expr))
    }

    fn transform_for(&mut self, exp_for: ExprForLoop) -> Expr {
        let pattern = &exp_for.pat;
        let pattern_str = prettify_pattern(pattern);
        let expr = &*exp_for.expr;
        let formatter = &self.formatter;
        let indent = (self.indent + 1) * TAB_SIZE;
        let mut stmts = vec![Stmt::Semi(Expr::Verbatim(quote! { writeln!(#formatter, "{}{} = {}:", " ".repeat(#indent), #pattern_str, #pattern).unwrap() }), Semi::default())];
        stmts.extend(exp_for.body.stmts);
        self.named_block(&format!("for {} in {}", pattern_str, prettify_expression_or_variants(expr)), Expr::ForLoop(ExprForLoop { body: Block { stmts, ..exp_for.body }, ..exp_for }))
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
        let cond_str = format!("if {}", prettify_expression_or_variants(&if_expr.cond));
        let cond = self.transform_expr(*if_expr.cond);
        let then_branch = self.transform_block(if_expr.then_branch);
        let else_branch = if_expr.else_branch.map(|x|
            match *x.1 {
                Expr::Block(b) => defaults::transform_block_expr(self, b),
                x => self.transform_expr(x)
            }
        );
        self.indent -= 1;
        let formatter = &self.formatter;
        quote_to_expr(quote! {
            {
                writeln!(#formatter, "{}{} {{", " ".repeat(#indent), #cond_str).unwrap();
                let cond = #cond;
                #[allow(unused_braces)]
                let t = if cond {
                    writeln!(#formatter, "{}then:", " ".repeat(#next_indent)).unwrap();
                    #then_branch
                } else {
                    writeln!(#formatter, "{}else:", " ".repeat(#next_indent)).unwrap();
                    #else_branch
                };
                writeln!(#formatter, "{}}}", " ".repeat(#indent)).unwrap();
                t
            }
        })
    }

    fn transform_match(&mut self, match_expr: ExprMatch) -> Expr {
        let expr = &*match_expr.expr;
        self.named_block(&format!("match {}", prettify_expression_or_variants(expr)), Expr::Match(match_expr))
    }

    fn transform_arm(&mut self, arm: Arm) -> Arm {
        let pat = &arm.pat;
        let pat = prettify_pattern(pat);
        let formatter = &self.formatter;
        let indent = self.indent * TAB_SIZE;
        let stmt = Stmt::Semi(quote_to_expr(quote!(writeln!(#formatter, "{}pattern {}:", " ".repeat(#indent), #pat).unwrap())), Semi::default());
        let arm_body = match *arm.body {
            Expr::Block(b) => {
                let mut stmts = vec![stmt];
                stmts.extend(b.block.stmts.into_iter().map(|x| self.transform_statement(x)));
                Expr::Block(ExprBlock { block: Block { stmts, ..b.block }, ..b })
            }
            e => {
                let e = self.transform_expr(e);
                quote_to_expr(quote!({ #stmt #e }))
            }
        };
        Arm {
            body: Box::new(arm_body),
            guard: arm.guard.map(|x| (x.0, transform_box_expr(self, x.1))),
            ..arm
        }
    }
}


impl CodeTransformer {
    fn named_block(&mut self, name: &str, expr: Expr) -> Expr {
        let indent = self.indent * TAB_SIZE;
        self.indent += 1;
        let transformed = defaults::transform_children(self, expr);
        self.indent -= 1;
        let name_space = if name.is_empty() { String::from(name) } else { format!("{name} ") };
        let formatter = &self.formatter;
        quote_to_expr(quote! {
            {
                writeln!(#formatter, "{}{}{{", " ".repeat(#indent), #name_space).unwrap();
                let t = #transformed;
                writeln!(#formatter, "{}}}", " ".repeat(#indent)).unwrap();
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
            UnOp::Neg(_) => match &*u.expr {
                Expr::Lit(l) => match l.lit {
                    Lit::Int(_) | Lit::Float(_) => false,
                    _ => true
                }
                _ => true
            },
            UnOp::Not(_) => true
        }
        Expr::Paren(p) => needs_intermediate_display(&p.expr),
        Expr::Group(g) => needs_intermediate_display(&g.expr),
        _ => false
    }
}

fn prettify_type(t: &Type) -> String {
    let string = prettify_expression_or_variants(quote!(todo!() as #t));
    let prefix = "todo!() as ";
    assert!(string.starts_with(prefix)); // need expression to format
    String::from(&string[prefix.len()..])
}

fn prettify_function(f: &Expr) -> String {
    let mut string = prettify_expression_or_variants(quote!(#f ()));
    let suffix = "()";
    assert!(string.ends_with(suffix)); // need expression to format
    string.pop();
    string.pop();
    string
}

fn prettify_pattern(p: &Pat) -> String {
    let mut string = prettify_expression_or_variants(quote!(if let #p = todo!() {}));
    let suffix = " = todo!() {}";
    assert!(string.ends_with(suffix)); // need expression to format
    string.truncate(string.len() - suffix.len());
    String::from(&string["if let ".len()..])
}

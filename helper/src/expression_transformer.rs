use syn::{Expr, ExprArray, ExprAssign, ExprAssignOp, ExprAsync, ExprAwait, ExprBinary, ExprBlock, Block, Stmt, Local, Item, ExprBox, ExprBreak, ExprCall, ExprCast, ExprClosure, ExprContinue, ExprField, ExprGroup, ExprForLoop, ExprIf, ExprIndex, ExprLet, ExprLit, ExprLoop, ExprMacro, ExprMatch, ExprMethodCall, ExprParen, ExprPath, ExprRange, ExprReference, ExprRepeat, ExprReturn, ExprStruct, ExprTry, ExprTryBlock, ExprTuple, ExprType, ExprUnary, ExprUnsafe, ExprWhile, ExprYield, Arm, FieldValue};
use syn::__private::TokenStream2;

pub trait ExprTransformer {
    fn transform_expr(&mut self, expr: Expr) -> Expr {
        match expr {
            Expr::Array(a) => self.transform_array(a),
            Expr::Assign(a) => self.transform_assign(a),
            Expr::AssignOp(ao) => self.transform_assign_op(ao),
            Expr::Async(a) => self.transform_async(a),
            Expr::Await(a) => self.transform_await(a),
            Expr::Binary(b) => self.transform_binary(b),
            Expr::Block(b) => self.transform_block_expr(b),
            Expr::Box(b) => self.transform_box(b),
            Expr::Break(b) => self.transform_break(b),
            Expr::Call(c) => self.transform_call(c),
            Expr::Cast(c) => self.transform_cast(c),
            Expr::Closure(c) => self.transform_closure(c),
            Expr::Continue(c) => self.transform_continue(c),
            Expr::Field(f) => self.transform_field(f),
            Expr::ForLoop(f) => self.transform_for(f),
            Expr::Group(g) => self.transform_group(g),
            Expr::If(i) => self.transform_if(i),
            Expr::Index(i) => self.transform_index(i),
            Expr::Let(l) => self.transform_let(l),
            Expr::Lit(l) => self.transform_literal(l),
            Expr::Loop(l) => self.transform_loop(l),
            Expr::Macro(m) => self.transform_macro(m),
            Expr::Match(m) => self.transform_match(m),
            Expr::MethodCall(mc) => self.transform_method_call(mc),
            Expr::Paren(p) => self.transform_parenthesis(p),
            Expr::Path(p) => self.transform_path(p),
            Expr::Range(r) => self.transform_range(r),
            Expr::Reference(r) => self.transform_reference(r),
            Expr::Repeat(r) => self.transform_repeat(r),
            Expr::Return(r) => self.transform_return(r),
            Expr::Struct(s) => self.transform_struct(s),
            Expr::Try(t) => self.transform_try(t),
            Expr::TryBlock(tb) => self.transform_try_block(tb),
            Expr::Tuple(t) => self.transform_tuple(t),
            Expr::Type(t) => self.transform_type(t),
            Expr::Unary(u) => self.transform_unary(u),
            Expr::Unsafe(u) => self.transform_unsafe(u),
            Expr::Verbatim(v) => self.transform_verbatim(v),
            Expr::While(w) => self.transform_while(w),
            Expr::Yield(y) => self.transform_yield(y),
            _ => panic!("Unexpected expression")
        }
    }

    fn transform_array(&mut self, array: ExprArray) -> Expr {
        defaults::transform_array(self, array)
    }

    fn transform_assign(&mut self, assign: ExprAssign) -> Expr {
        defaults::transform_assign(self, assign)
    }

    fn transform_assign_op(&mut self, assign: ExprAssignOp) -> Expr {
        defaults::transform_assign_op(self, assign)
    }

    fn transform_async(&mut self, async_expr: ExprAsync) -> Expr {
        defaults::transform_async(self, async_expr)
    }

    fn transform_await(&mut self, await_expr: ExprAwait) -> Expr {
        defaults::transform_await(self, await_expr)
    }

    fn transform_binary(&mut self, binary: ExprBinary) -> Expr {
        defaults::transform_binary(self, binary)
    }

    fn transform_block_expr(&mut self, block_expr: ExprBlock) -> Expr {
        defaults::transform_block_expr(self, block_expr)
    }

    fn transform_block(&mut self, block: Block) -> Block {
        defaults::transform_block(self, block)
    }

    fn transform_statement(&mut self, statement: Stmt) -> Stmt {
        defaults::transform_statement(self, statement)
    }

    fn transform_local(&mut self, local: Local) -> Stmt {
        defaults::transform_local(self, local)
    }

    fn transform_item(&mut self, item: Item) -> Stmt {
        defaults::transform_item(self, item)
    }

    fn transform_box(&mut self, box_expr: ExprBox) -> Expr {
        defaults::transform_box(self, box_expr)
    }

    fn transform_break(&mut self, break_expr: ExprBreak) -> Expr {
        defaults::transform_break(self, break_expr)
    }

    fn transform_call(&mut self, call: ExprCall) -> Expr {
        defaults::transform_call(self, call)
    }

    fn transform_cast(&mut self, cast: ExprCast) -> Expr {
        defaults::transform_cast(self, cast)
    }

    fn transform_closure(&mut self, closure: ExprClosure) -> Expr {
        defaults::transform_closure(self, closure)
    }

    fn transform_continue(&mut self, continue_expr: ExprContinue) -> Expr {
        defaults::transform_continue(self, continue_expr)
    }

    fn transform_field(&mut self, field: ExprField) -> Expr {
        defaults::transform_field(self, field)
    }

    fn transform_for(&mut self, exp_for: ExprForLoop) -> Expr {
        defaults::transform_for(self, exp_for)
    }

    fn transform_group(&mut self, group: ExprGroup) -> Expr {
        defaults::transform_group(self, group)
    }

    fn transform_if(&mut self, if_expr: ExprIf) -> Expr {
        defaults::transform_if(self, if_expr)
    }

    fn transform_index(&mut self, index: ExprIndex) -> Expr {
        defaults::transform_index(self, index)
    }

    fn transform_let(&mut self, let_expr: ExprLet) -> Expr {
        defaults::transform_let(self, let_expr)
    }

    fn transform_literal(&mut self, literal: ExprLit) -> Expr {
        defaults::transform_literal(self, literal)
    }

    fn transform_loop(&mut self, loop_expr: ExprLoop) -> Expr {
        defaults::transform_loop(self, loop_expr)
    }

    fn transform_macro(&mut self, macro_expr: ExprMacro) -> Expr {
        defaults::transform_macro(self, macro_expr)
    }

    fn transform_match(&mut self, match_expr: ExprMatch) -> Expr {
        defaults::transform_match(self, match_expr)
    }

    fn transform_arm(&mut self, arm: Arm) -> Arm {
        defaults::transform_arm(self, arm)
    }

    fn transform_method_call(&mut self, method_call: ExprMethodCall) -> Expr {
        defaults::transform_method_call(self, method_call)
    }

    fn transform_parenthesis(&mut self, parenthesis: ExprParen) -> Expr {
        defaults::transform_parenthesis(self, parenthesis)
    }

    fn transform_path(&mut self, path: ExprPath) -> Expr {
        defaults::transform_path(self, path)
    }

    fn transform_range(&mut self, range: ExprRange) -> Expr {
        defaults::transform_range(self, range)
    }

    fn transform_reference(&mut self, reference: ExprReference) -> Expr {
        defaults::transform_reference(self, reference)
    }

    fn transform_repeat(&mut self, repeat: ExprRepeat) -> Expr {
        defaults::transform_repeat(self, repeat)
    }

    fn transform_return(&mut self, return_expr: ExprReturn) -> Expr {
        defaults::transform_return(self, return_expr)
    }

    fn transform_struct(&mut self, struct_expr: ExprStruct) -> Expr {
        defaults::transform_struct(self, struct_expr)
    }

    fn transform_field_value(&mut self, field_value: FieldValue) -> FieldValue {
        defaults::transform_field_value(self, field_value)
    }

    fn transform_try(&mut self, try_expr: ExprTry) -> Expr {
        defaults::transform_try(self, try_expr)
    }

    fn transform_try_block(&mut self, try_block: ExprTryBlock) -> Expr {
        defaults::transform_try_block(self, try_block)
    }

    fn transform_tuple(&mut self, tuple: ExprTuple) -> Expr {
        defaults::transform_tuple(self, tuple)
    }

    fn transform_type(&mut self, type_expr: ExprType) -> Expr {
        defaults::transform_type(self, type_expr)
    }

    fn transform_unary(&mut self, unary: ExprUnary) -> Expr {
        defaults::transform_unary(self, unary)
    }

    fn transform_unsafe(&mut self, unsafe_expr: ExprUnsafe) -> Expr {
        defaults::transform_unsafe(self, unsafe_expr)
    }

    fn transform_verbatim(&mut self, verbatim: TokenStream2) -> Expr {
        defaults::transform_verbatim(self, verbatim)
    }

    fn transform_while(&mut self, while_expr: ExprWhile) -> Expr {
        defaults::transform_while(self, while_expr)
    }

    fn transform_yield(&mut self, yield_expr: ExprYield) -> Expr {
        defaults::transform_yield(self, yield_expr)
    }
}

pub mod defaults {
    use super::ExprTransformer;
    use syn::{Expr, ExprArray, ExprAssign, ExprAssignOp, ExprAsync, ExprAwait, ExprBinary, ExprBlock, Block, Stmt, Local, Item, ExprBox, ExprBreak, ExprCall, ExprCast, ExprClosure, ExprContinue, ExprField, ExprGroup, ExprForLoop, ExprIf, ExprIndex, ExprLet, ExprLit, ExprLoop, ExprMacro, ExprMatch, ExprMethodCall, ExprParen, ExprPath, ExprRange, ExprReference, ExprRepeat, ExprReturn, ExprStruct, ExprTry, ExprTryBlock, ExprTuple, ExprType, ExprUnary, ExprUnsafe, ExprWhile, ExprYield, Arm, FieldValue};
    use syn::__private::TokenStream2;
    use super::transform_box_expr;

    pub fn transform_children(trans: &mut (impl ExprTransformer + ?Sized), expr: Expr) -> Expr {
        match expr {
            Expr::Array(a) => transform_array(trans,a),
            Expr::Assign(a) => transform_assign(trans, a),
            Expr::AssignOp(ao) => transform_assign_op(trans, ao),
            Expr::Async(a) => transform_async(trans, a),
            Expr::Await(a) => transform_await(trans, a),
            Expr::Binary(b) => transform_binary(trans, b),
            Expr::Block(b) => transform_block_expr(trans, b),
            Expr::Box(b) => transform_box(trans, b),
            Expr::Break(b) => transform_break(trans, b),
            Expr::Call(c) => transform_call(trans, c),
            Expr::Cast(c) => transform_cast(trans, c),
            Expr::Closure(c) => transform_closure(trans, c),
            Expr::Continue(c) => transform_continue(trans, c),
            Expr::Field(f) => transform_field(trans, f),
            Expr::ForLoop(f) => transform_for(trans, f),
            Expr::Group(g) => transform_group(trans, g),
            Expr::If(i) => transform_if(trans, i),
            Expr::Index(i) => transform_index(trans, i),
            Expr::Let(l) => transform_let(trans, l),
            Expr::Lit(l) => transform_literal(trans, l),
            Expr::Loop(l) => transform_loop(trans, l),
            Expr::Macro(m) => transform_macro(trans, m),
            Expr::Match(m) => transform_match(trans, m),
            Expr::MethodCall(mc) => transform_method_call(trans, mc),
            Expr::Paren(p) => transform_parenthesis(trans, p),
            Expr::Path(p) => transform_path(trans, p),
            Expr::Range(r) => transform_range(trans, r),
            Expr::Reference(r) => transform_reference(trans, r),
            Expr::Repeat(r) => transform_repeat(trans, r),
            Expr::Return(r) => transform_return(trans, r),
            Expr::Struct(s) => transform_struct(trans, s),
            Expr::Try(t) => transform_try(trans, t),
            Expr::TryBlock(tb) => transform_try_block(trans, tb),
            Expr::Tuple(t) => transform_tuple(trans, t),
            Expr::Type(t) => transform_type(trans, t),
            Expr::Unary(u) => transform_unary(trans, u),
            Expr::Unsafe(u) => transform_unsafe(trans, u),
            Expr::Verbatim(v) => transform_verbatim(trans, v),
            Expr::While(w) => transform_while(trans, w),
            Expr::Yield(y) => transform_yield(trans, y),
            _ => panic!("Unexpected expression")
        }
    }

    pub fn transform_array(trans: &mut (impl ExprTransformer + ?Sized), array: ExprArray) -> Expr {
        let elems = array.elems.into_iter().map(|x| trans.transform_expr(x)).collect();
        Expr::Array(ExprArray { elems, ..array })
    }

    pub fn transform_assign(trans: &mut (impl ExprTransformer + ?Sized), assign: ExprAssign) -> Expr {
        Expr::Assign(ExprAssign {
            left: transform_box_expr(trans, assign.left),
            right: transform_box_expr(trans, assign.right),
            ..assign
        })
    }

    pub fn transform_assign_op(trans: &mut (impl ExprTransformer + ?Sized), assign: ExprAssignOp) -> Expr {
        Expr::AssignOp(ExprAssignOp {
            left: transform_box_expr(trans, assign.left),
            right: transform_box_expr(trans, assign.right),
            ..assign
        })
    }

    pub fn transform_async(trans: &mut (impl ExprTransformer + ?Sized), async_expr: ExprAsync) -> Expr {
        Expr::Async(ExprAsync {
            block: trans.transform_block(async_expr.block),
            ..async_expr
        })
    }

    pub fn transform_await(trans: &mut (impl ExprTransformer + ?Sized), await_expr: ExprAwait) -> Expr {
        Expr::Await(ExprAwait {
            base: transform_box_expr(trans, await_expr.base),
            ..await_expr
        })
    }

    pub fn transform_binary(trans: &mut (impl ExprTransformer + ?Sized), binary: ExprBinary) -> Expr {
        Expr::Binary(ExprBinary {
            left: transform_box_expr(trans, binary.left),
            right: transform_box_expr(trans, binary.right),
            ..binary
        })
    }

    pub fn transform_block_expr(trans: &mut (impl ExprTransformer + ?Sized), block_expr: ExprBlock) -> Expr {
        Expr::Block(ExprBlock {
            block: trans.transform_block(block_expr.block),
            ..block_expr
        })
    }

    pub fn transform_block(trans: &mut (impl ExprTransformer + ?Sized), block: Block) -> Block {
        Block {
            stmts: block.stmts.into_iter().map(|x| trans.transform_statement(x)).collect(),
            ..block
        }
    }

    pub fn transform_statement(trans: &mut (impl ExprTransformer + ?Sized), statement: Stmt) -> Stmt {
        match statement {
            Stmt::Local(l) => trans.transform_local(l),
            Stmt::Expr(e) => Stmt::Expr(trans.transform_expr(e)),
            Stmt::Semi(e, t) => Stmt::Semi(trans.transform_expr(e), t),
            Stmt::Item(i) => trans.transform_item(i),
        }
    }

    pub fn transform_local(trans: &mut (impl ExprTransformer + ?Sized), local: Local) -> Stmt {
        let init = local.init.map(
            |(eq, expr)| (eq, transform_box_expr(trans, expr))
        );
        Stmt::Local(Local { init, ..local })
    }

    pub fn transform_item(_: &mut (impl ExprTransformer + ?Sized), item: Item) -> Stmt {
        Stmt::Item(item)
    }

    pub fn transform_box(trans: &mut (impl ExprTransformer + ?Sized), box_expr: ExprBox) -> Expr {
        Expr::Box(ExprBox {
            expr: transform_box_expr(trans, box_expr.expr),
            ..box_expr
        })
    }

    pub fn transform_break(trans: &mut (impl ExprTransformer + ?Sized), break_expr: ExprBreak) -> Expr {
        Expr::Break(ExprBreak {
            expr: break_expr.expr.map(|x| transform_box_expr(trans, x)),
            ..break_expr
        })
    }

    pub fn transform_call(trans: &mut (impl ExprTransformer + ?Sized), call: ExprCall) -> Expr {
        let func = transform_box_expr(trans, call.func);
        let args = call.args.into_iter()
            .map(|x| trans.transform_expr(x))
            .collect();
        Expr::Call(ExprCall { func, args, ..call })
    }

    pub fn transform_cast(trans: &mut (impl ExprTransformer + ?Sized), cast: ExprCast) -> Expr {
        Expr::Cast(ExprCast {
            expr: transform_box_expr(trans, cast.expr),
            ..cast
        })
    }

    pub fn transform_closure(trans: &mut (impl ExprTransformer + ?Sized), closure: ExprClosure) -> Expr {
        Expr::Closure(ExprClosure {
            body: transform_box_expr(trans, closure.body),
            ..closure
        })
    }

    pub fn transform_continue(_: &mut (impl ExprTransformer + ?Sized), continue_expr: ExprContinue) -> Expr {
        Expr::Continue(continue_expr)
    }

    pub fn transform_field(trans: &mut (impl ExprTransformer + ?Sized), field: ExprField) -> Expr {
        Expr::Field(ExprField {
            base: transform_box_expr(trans, field.base),
            ..field
        })
    }

    pub fn transform_for(trans: &mut (impl ExprTransformer + ?Sized), exp_for: ExprForLoop) -> Expr {
        Expr::ForLoop(ExprForLoop {
            expr: transform_box_expr(trans, exp_for.expr),
            body: trans.transform_block(exp_for.body),
            ..exp_for
        })
    }

    pub fn transform_group(trans: &mut (impl ExprTransformer + ?Sized), group: ExprGroup) -> Expr {
        Expr::Group(ExprGroup {
            expr: transform_box_expr(trans, group.expr),
            ..group
        })
    }

    pub fn transform_if(trans: &mut (impl ExprTransformer + ?Sized), if_expr: ExprIf) -> Expr {
        let cond = transform_box_expr(trans, if_expr.cond);
        let then_branch = trans.transform_block(if_expr.then_branch);
        let else_branch = if_expr.else_branch.map(|(else_token, x)|
            (else_token, transform_box_expr(trans, x))
        );
        Expr::If(ExprIf { cond, then_branch, else_branch, ..if_expr })
    }

    pub fn transform_index(trans: &mut (impl ExprTransformer + ?Sized), index: ExprIndex) -> Expr {
        Expr::Index(ExprIndex {
            expr: transform_box_expr(trans, index.expr),
            index: transform_box_expr(trans, index.index),
            ..index
        })
    }

    pub fn transform_let(trans: &mut (impl ExprTransformer + ?Sized), let_expr: ExprLet) -> Expr {
        Expr::Let(ExprLet {
            expr: transform_box_expr(trans, let_expr.expr),
            ..let_expr
        })
    }

    pub fn transform_literal(_: &mut (impl ExprTransformer + ?Sized), literal: ExprLit) -> Expr {
        Expr::Lit(literal)
    }

    pub fn transform_loop(trans: &mut (impl ExprTransformer + ?Sized), loop_expr: ExprLoop) -> Expr {
        Expr::Loop(ExprLoop {
            body: trans.transform_block(loop_expr.body),
            ..loop_expr
        })
    }

    pub fn transform_macro(_: &mut (impl ExprTransformer + ?Sized), macro_expr: ExprMacro) -> Expr {
        Expr::Macro(macro_expr)
    }

    pub fn transform_match(trans: &mut (impl ExprTransformer + ?Sized), match_expr: ExprMatch) -> Expr {
        Expr::Match(ExprMatch {
            expr: transform_box_expr(trans, match_expr.expr),
            arms: match_expr.arms.into_iter().map(|x| trans.transform_arm(x)).collect(),
            ..match_expr
        })
    }

    pub fn transform_arm(trans: &mut (impl ExprTransformer + ?Sized), arm: Arm) -> Arm {
        Arm {
            guard: arm.guard.map(|(i, g)| (i, transform_box_expr(trans, g))),
            body: transform_box_expr(trans, arm.body),
            ..arm
        }
    }

    pub fn transform_method_call(trans: &mut (impl ExprTransformer + ?Sized), method_call: ExprMethodCall) -> Expr {
        Expr::MethodCall(ExprMethodCall {
            receiver: transform_box_expr(trans, method_call.receiver),
            args: method_call.args.into_iter().map(|x| trans.transform_expr(x)).collect(),
            ..method_call
        })
    }

    pub fn transform_parenthesis(trans: &mut (impl ExprTransformer + ?Sized), parenthesis: ExprParen) -> Expr {
        Expr::Paren(ExprParen {
            expr: transform_box_expr(trans, parenthesis.expr),
            ..parenthesis
        })
    }

    pub fn transform_path(_: &mut (impl ExprTransformer + ?Sized), path: ExprPath) -> Expr {
        Expr::Path(path)
    }

    pub fn transform_range(trans: &mut (impl ExprTransformer + ?Sized), range: ExprRange) -> Expr {
        Expr::Range(ExprRange {
            from: range.from.map(|x| transform_box_expr(trans, x)),
            to: range.to.map(|x| transform_box_expr(trans, x)),
            ..range
        })
    }

    pub fn transform_reference(trans: &mut (impl ExprTransformer + ?Sized), reference: ExprReference) -> Expr {
        Expr::Reference(ExprReference {
            expr: transform_box_expr(trans, reference.expr),
            ..reference
        })
    }

    pub fn transform_repeat(trans: &mut (impl ExprTransformer + ?Sized), repeat: ExprRepeat) -> Expr {
        Expr::Repeat(ExprRepeat {
            expr: transform_box_expr(trans, repeat.expr),
            len: transform_box_expr(trans, repeat.len),
            ..repeat
        })
    }

    pub fn transform_return(trans: &mut (impl ExprTransformer + ?Sized), return_expr: ExprReturn) -> Expr {
        Expr::Return(ExprReturn {
            expr: return_expr.expr.map(|x| transform_box_expr(trans, x)),
            ..return_expr
        })
    }

    pub fn transform_struct(trans: &mut (impl ExprTransformer + ?Sized), struct_expr: ExprStruct) -> Expr {
        Expr::Struct(ExprStruct {
            fields: struct_expr.fields.into_iter().map(|x| trans.transform_field_value(x)).collect(),
            rest: struct_expr.rest.map(|x| transform_box_expr(trans, x)),
            ..struct_expr
        })
    }

    pub fn transform_field_value(trans: &mut (impl ExprTransformer + ?Sized), field_value: FieldValue) -> FieldValue {
        FieldValue {
            expr: trans.transform_expr(field_value.expr),
            ..field_value
        }
    }

    pub fn transform_try(trans: &mut (impl ExprTransformer + ?Sized), try_expr: ExprTry) -> Expr {
        Expr::Try(ExprTry {
            expr: transform_box_expr(trans, try_expr.expr),
            ..try_expr
        })
    }

    pub fn transform_try_block(trans: &mut (impl ExprTransformer + ?Sized), try_block: ExprTryBlock) -> Expr {
        Expr::TryBlock(ExprTryBlock {
            block: trans.transform_block(try_block.block),
            ..try_block
        })
    }

    pub fn transform_tuple(trans: &mut (impl ExprTransformer + ?Sized), tuple: ExprTuple) -> Expr {
        Expr::Tuple(ExprTuple {
            elems: tuple.elems.into_iter().map(|x| trans.transform_expr(x)).collect(),
            ..tuple
        })
    }

    pub fn transform_type(trans: &mut (impl ExprTransformer + ?Sized), type_expr: ExprType) -> Expr {
        Expr::Type(ExprType {
            expr: transform_box_expr(trans, type_expr.expr),
            ..type_expr
        })
    }

    pub fn transform_unary(trans: &mut (impl ExprTransformer + ?Sized), unary: ExprUnary) -> Expr {
        Expr::Unary(ExprUnary {
            expr: transform_box_expr(trans, unary.expr),
            ..unary
        })
    }

    pub fn transform_unsafe(trans: &mut (impl ExprTransformer + ?Sized), unsafe_expr: ExprUnsafe) -> Expr {
        Expr::Unsafe(ExprUnsafe {
            block: trans.transform_block(unsafe_expr.block),
            ..unsafe_expr
        })
    }

    pub fn transform_verbatim(_: &mut (impl ExprTransformer + ?Sized), verbatim: TokenStream2) -> Expr {
        Expr::Verbatim(verbatim)
    }

    pub fn transform_while(trans: &mut (impl ExprTransformer + ?Sized), while_expr: ExprWhile) -> Expr {
        Expr::While(ExprWhile {
            cond: transform_box_expr(trans, while_expr.cond),
            body: trans.transform_block(while_expr.body),
            ..while_expr
        })
    }

    pub fn transform_yield(trans: &mut (impl ExprTransformer + ?Sized), yield_expr: ExprYield) -> Expr {
        Expr::Yield(ExprYield {
            expr: yield_expr.expr.map(|x| transform_box_expr(trans, x)),
            ..yield_expr
        })
    }
}

pub fn transform_box_expr<T: ExprTransformer + ?Sized>(transformer: &mut T, mut b: Box<Expr>) -> Box<Expr> {
    *b = transformer.transform_expr(*b);
    b
}
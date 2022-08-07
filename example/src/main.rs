use std::ops::Mul;
use intermediate_expr_macro::{explain_stdout};

struct P<T> {
    f: T,
    s: T,
}

fn f<T: Mul<i32, Output=T>>(x: T) -> T { x * 2 }

fn g<T: Mul<T, Output=T>>(x: T, y: T) -> T { x * y }

fn h<T: Default>() -> T { T::default() }

type MyType = i32;

#[allow(unused_unsafe)]
fn main() {
    println!("{}", explain_stdout!(-(2 * 5 + 3) * (7 + 6)));
    println!("{}", explain_stdout!(String::from("str1\nstr2") + "str3"));
    let mut x = String::from("str1\nstr2");
    explain_stdout!(x += "str3");
    let mut x = 5;
    explain_stdout!(x += 3);
    println!("??: {}", x);
    explain_stdout!(x += 3+3);
    explain_stdout!(2 as i64);
    explain_stdout!(2 as MyType);
    explain_stdout!((2, 3).0 as i64);
    let x = (2, 3);
    explain_stdout!(x.0 as i64);
    explain_stdout!(P { f: 0, s: 1}.f as i64);
    explain_stdout!(P { f: 0, s: 1+1}.s);
    let x = [1, 1, 2, 3, 5, 8, 13, 21, 34, 55];
    explain_stdout!(x[5] as i64);
    explain_stdout!(x[2+5] as i64);
    explain_stdout!(f(2+3));
    explain_stdout!(g(2+3, 4+5));
    explain_stdout!(f::<i32>(2+3));
    explain_stdout!(h::<i32>());
    explain_stdout!({
        2+2;
        while (3+4==8) {
        }
        for i in 1..3 {
            2+5;
        }
        5+6;
        loop {
            10+2;
            break 20+4;
        };
        if 2 == 3 {
            4+5;
        } else {
            4+6;
        };
        if 2 == 2 {
            4+5;
        } else {
            4+6;
        };
        unsafe {
            2+2;
        };
        match 2 {
            3 => (),
            _ => ()
        };
    });
    explain_stdout!(String::from("ç¬"));
}

#[cfg(test)]
mod tests {
    use std::fmt::{Display, Formatter};
    use intermediate_expr_macro::explain_string;

    #[test]
    fn arithmetic() {
        let res = explain_string!(-(2 * 5 + 3) * (7 + 6));
        assert_eq!(res.0, "\
1. 2 * 5 => 10
2. 2 * 5 + 3 => 10 + 3 => 13
3. -(2 * 5 + 3) => -(13) => -13
4. 7 + 6 => 13
5. -(2 * 5 + 3) * (7 + 6) => -13 * 13 => -169
"
        );
        assert_eq!(res.1, -169);
    }

    #[test]
    fn multi_line() {
        let res = explain_string!(String::from("str1\nstr2") + "str3");
        let expected = &r#"
1. String::from("str1\nstr2") => str1
                                 str2
2. String::from("str1\nstr2") + "str3" => str1 + str3 => str1
                                          str2           str2str3
"#[1..];
        assert_eq!(res.0, expected);
        assert_eq!(res.1, String::from("str1\nstr2str3"));
    }

    #[test]
    fn multi_line_mutating() {
        let mut x = String::from("str1\nstr2");
        let res = explain_string!(x += "str3");
        let expected = &r#"
1. (x += "str3") => (x/* = str1    += str3) => str1
                           str2 */             str2str3
"#[1..];
        assert_eq!(res.0, expected);
        assert_eq!(x, String::from("str1\nstr2str3"));
        assert_eq!(res.1, ());
    }

    #[test]
    fn mutating_after_eval() {
        let mut y = 5;
        let res = explain_string!(y += 2+8);
        let expected = &r"
1. 2 + 8 => 10
2. (y += 2 + 8) => (y/* = 5 */ += 10) => 15
"[1..];
        assert_eq!(res.0, expected);
        assert_eq!(y, 15);
        assert_eq!(res.1, ());
    }

    #[test]
    fn mutating_ref_after_eval() {
        let mut y = 5;
        let res = explain_string!({
            let m: &mut i32 = &mut y;
            *m += 2+8;
        });
        assert_eq!(y, 15);
        let expected = "\
{
    1. 2 + 8 => 10
    2. (*m += 2 + 8) => (*m/* = 5 */ += 10) => 15
}
";
        assert_eq!(res.0, expected);
        assert_eq!(res.1, ());
    }

    type MyType = u8;

    #[test]
    fn cast() {
        let res = explain_string!( -1i32 as u8);
        assert_eq!(res.0, "1. -1i32 as u8 => 255\n");
        assert_eq!(res.1, 255);
    }

    #[test]
    fn cast_typealias() {
        let res = explain_string!( -1i32 as MyType);
        assert_eq!(res.0, "1. -1i32 as MyType => 255\n");
        assert_eq!(res.1, 255);
    }

    #[test]
    fn cast_anon_field_access() {
        let res = explain_string!( (2, 3).0);
        assert_eq!(res.0, "1. (2, 3).0 => 2\n");
        assert_eq!(res.1, 2);
    }

    #[test]
    fn cast_anon_field_access_extracted() {
        let p = (2, 3);
        let res = explain_string!( p.0);
        assert_eq!(res.0, "1. p.0 => 2\n");
        assert_eq!(res.1, 2);
    }

    struct Wrapper<T> { pub value: T }

    #[test]
    fn cast_named_field_access() {
        let res = explain_string!( Wrapper { value: 2 }.value);
        assert_eq!(res.0, "1. Wrapper { value: 2 }.value => 2\n");
        assert_eq!(res.1, 2);
    }

    #[test]
    fn cast_named_field_access_extracted() {
        let p = Wrapper { value: 2 };
        let res = explain_string!(p.value);
        assert_eq!(res.0, "1. p.value => 2\n");
        assert_eq!(res.1, 2);
    }

    #[test]
    fn array_access() {
        let x = [1, 1, 2, 3, 5, 8, 13, 21, 34, 55];
        let res = explain_string!(x[2+4]);
        assert_eq!(res.0, "1. 2 + 4 => 6\n2. x[2 + 4] => x[6] => 13\n");
        assert_eq!(res.1, 13);
    }

    fn arg0<T: Default>() -> T { T::default() }
    fn arg0_i() -> i32 { 0 }
    fn arg1<T>(x: T) -> T { x }
    fn arg2<T>(x: T, _: T) -> T { x }

    #[test]
    fn function_0() {
        let res = explain_string!(arg0_i());
        assert_eq!(res.0, "1. arg0_i() => 0\n");
        assert_eq!(res.1, 0);
    }

    #[test]
    fn function_0_turbofish() {
        let res = explain_string!(arg0::<i32>());
        assert_eq!(res.0, "1. arg0::<i32>() => 0\n");
        assert_eq!(res.1, 0);
    }

    #[test]
    fn function_1() {
        let res = explain_string!(arg1(2 + 3));
        let expected = "\
1. 2 + 3 => 5
2. arg1(2 + 3) => arg1(5) => 5
";
        assert_eq!(res.0, expected);
        assert_eq!(res.1, 5);
    }

    #[test]
    fn function_1_turbofish() {
        let res = explain_string!(arg1::<i32>(2 + 3));
        let expected = "\
1. 2 + 3 => 5
2. arg1::<i32>(2 + 3) => arg1::<i32>(5) => 5
";
        assert_eq!(res.0, expected);
        assert_eq!(res.1, 5);
    }

    #[test]
    fn function_2() {
        let res = explain_string!(arg2(2 + 3, 4 + 5));
        let expected = "\
1. 2 + 3 => 5
2. 4 + 5 => 9
3. arg2(2 + 3, 4 + 5) => arg2(5, 9) => 5
";
        assert_eq!(res.0, expected);
        assert_eq!(res.1, 5);
    }

    #[test]
    fn function_2_turbofish() {
        let res = explain_string!(arg2::<i32>(2 + 3, 4 + 5));
        let expected = "\
1. 2 + 3 => 5
2. 4 + 5 => 9
3. arg2::<i32>(2 + 3, 4 + 5) => arg2::<i32>(5, 9) => 5
";
        assert_eq!(res.0, expected);
        assert_eq!(res.1, 5);
    }

    #[test]
    fn block() {
        let res = explain_string!({ 2 + 2 });
        let expected = "\
{
    1. 2 + 2 => 4
}
";
        assert_eq!(res.0, expected);
        assert_eq!(res.1, 4);
    }

    #[test]
    fn nested_block() {
        let res = explain_string!({{ 2 + 2 }});
        let expected = "\
{
    {
        1. 2 + 2 => 4
    }
}
";
        assert_eq!(res.0, expected);
        assert_eq!(res.1, 4);
    }

    #[test]
    fn while_block() {
        let res = explain_string!(while 2 + 3 == 5 { 2 + 2; break });
        let expected = "\
while 2 + 3 == 5 {
    1. 2 + 3 => 5
    2. 2 + 3 == 5 => 5 == 5 => true
    3. 2 + 2 => 4
}
";
        assert_eq!(res.0, expected);
    }

    #[test]
    fn for_block() {
        let res = explain_string!(for i in 0..3 { 2 + i; });
        let expected = "\
for i in 0..3 {
    i = 0:
    1. 2 + i => 2
    i = 1:
    1. 2 + i => 3
    i = 2:
    1. 2 + i => 4
}
";
        assert_eq!(res.0, expected);
    }

    #[test]
    fn loop_block() {
        let res = explain_string!(loop { 2 + 2; break });
        let expected = "\
loop {
    1. 2 + 2 => 4
}
";
        assert_eq!(res.0, expected);
    }

    #[test]
    fn loop_block_returning() {
        let res = explain_string!(loop { 2 + 2; break 10 + 10 });
        let expected = "\
loop {
    1. 2 + 2 => 4
    2. 10 + 10 => 20
}
";
        assert_eq!(res.0, expected);
        assert_eq!(res.1, 20);
    }

    #[test]
    fn short_if_false_block() {
        let res = explain_string!(if 2 == 3 { 2 + 2; });
        let expected = "\
if 2 == 3 {
    1. 2 == 3 => false
    else:
}
";
        assert_eq!(res.0, expected);
    }

    #[test]
    fn short_if_true_block() {
        let res = explain_string!(if 2 == 2 { 2 + 2; });
        let expected = "\
if 2 == 2 {
    1. 2 == 2 => true
    then:
    2. 2 + 2 => 4
}
";
        assert_eq!(res.0, expected);
    }

    #[test]
    fn if_false_block() {
        let res = explain_string!(if 2 == 3 { 2 + 2; } else { 3 + 3; });
        let expected = "\
if 2 == 3 {
    1. 2 == 3 => false
    else:
    3. 3 + 3 => 6
}
";
        assert_eq!(res.0, expected);
    }

    #[test]
    fn if_true_block() {
        let res = explain_string!(if 2 == 2 { 2 + 2; } else { 3 + 3;});
        let expected = "\
if 2 == 2 {
    1. 2 == 2 => true
    then:
    2. 2 + 2 => 4
}
";
        assert_eq!(res.0, expected);
    }

    struct S<T>(T);

    impl<T> Display for S<T> {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            write!(f, "S")
        }
    }

    impl<T> S<T> {
        fn f(&self) -> bool { true }
        fn g<S>(&self) -> bool { true }
    }

    #[test]
    fn if_false_block_returning() {
        let res =
            explain_string!(if 2 == 3 && (S::<i32>(32) as S<i32>).f() { 2 + 2 } else { 3 + 3 });
        let expected = "\
if 2 == 3 && (S::<i32>(32) as S<i32>).f() {
    1. 2 == 3 => false
    5. 2 == 3 && (S::<i32>(32) as S<i32>).f() => false && ... => false
    else:
    7. 3 + 3 => 6
}
";
        assert_eq!(res.0, expected);
        assert_eq!(res.1, 6);
    }

    #[test]
    fn if_true_block_returning() {
        let res = explain_string!(if 2 == 2 && (false || true || false) { 2 + 2 } else { 3 + 3 });
        let expected = "\
if 2 == 2 && (false || true || false) {
    1. 2 == 2 => true
    2. false || true => true
    3. false || true || false => true || ... => true
    4. 2 == 2 && (false || true || false) => true && true => true
    then:
    5. 2 + 2 => 4
}
";
        assert_eq!(res.0, expected);
        assert_eq!(res.1, 4);
    }

    #[test]
    #[allow(unused_unsafe)]
    fn unsafe_block() {
        let res = explain_string!(unsafe { ((2)) + 2 });
        let expected = "\
unsafe {
    1. ((2)) + 2 => 4
}
";
        assert_eq!(res.0, expected);
        assert_eq!(res.1, 4);
    }

    #[test]
    fn match_block() {
        let res = explain_string!(match 2 + 2 {
            5 => { 10 + 10; },
            4 => { 10 - 10; },
            _ => { 10 * 10; }
        });
        let expected = "\
match 2 + 2 {
    1. 2 + 2 => 4
    pattern 4:
    3. 10 - 10 => 0
}
";
        assert_eq!(res.0, expected);
    }

    #[test]
    fn match_block_returning() {
        let res = explain_string!(match 2 + 2 {
            5 => 10 + 10,
            4 => 10 - 10,
            _ => 10 * 10
        });
        let expected = "\
match 2 + 2 {
    1. 2 + 2 => 4
    pattern 4:
    3. 10 - 10 => 0
}
";
        assert_eq!(res.0, expected);
        assert_eq!(res.1, 0);
    }

    #[test]
    fn unicode() {
        let res = explain_string!(String::from("ç"));
        let expected = r#"1. String::from("ç") => ç
"#;
        assert_eq!(res.0, expected);
        assert_eq!(res.1, String::from("ç"));
    }

    #[test]
    fn method() {
        let res = explain_string!(S(2).g::<i64>());
        let expected = &r#"
1. S(2) => S
2. S(2).g::<i64>() => S.g::<i64>() => true
"#[1..];
        assert_eq!(res.0, expected);
        assert_eq!(res.1, true);
    }
}

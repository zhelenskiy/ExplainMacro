use std::ops::Mul;
use intermediate_expr_macro::explain;

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
    println!("{}", explain!(-(2 * 5 + 3) * (7 + 6)));
    println!("{}", explain!(String::from("kek\nlol") + "bruh"));
    let mut x = String::from("kek\nlol");
    explain!(x += "bruh");
    let mut x = 5;
    explain!(x += 3);
    println!("??: {}", x);
    explain!(x += 3+3);
    explain!(2 as i64);
    explain!(2 as MyType);
    explain!((2, 3).0 as i64);
    let x = (2, 3);
    explain!(x.0 as i64);
    explain!(P { f: 0, s: 1}.f as i64);
    explain!(P { f: 0, s: 1+1}.s);
    let x = [1, 1, 2, 3, 5, 8, 13, 21, 34, 55];
    explain!(x[5] as i64);
    explain!(x[2+5] as i64);
    explain!(f(2+3));
    explain!(g(2+3, 4+5));
    explain!(f::<i32>(2+3));
    explain!(h::<i32>());
    explain!({
        2+2;
        while (3+4==8) {
        }
        for _i in 1..3 {
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
}

use std::ops::Mul;
use intermediate_expr_macro::explain;

struct P<T> {
    f: T,
    s: T,
}

fn f<T: Mul<i32, Output=T>>(x: T) -> T { x * 2 }

type my_type = i32;

fn main() {
    // println!("{}", explain!(-(2 * 5 + 3) * (7 + 6)));
    // println!("{}", explain!(String::from("kek\nlol") + "bruh"));
    let mut x = 5;
    explain!(x += 3);
    explain!(x += 3+3);
    explain!(2 as i64);
    explain!(2 as my_type);
    explain!((2, 3).0 as i64);
    let x = (2, 3);
    explain!(x.0 as i64);
    explain!(P { f: 0, s: 1}.f as i64);
    explain!(P { f: 0, s: 1+1}.s);
    let x = [1, 1, 2, 3, 5, 8, 13, 21, 34, 55];
    explain!(x[5] as i64);
    explain!(x[2+5] as i64);
    explain!(f(2+3));
    explain!(f::<i32>(2+3));
}

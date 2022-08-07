# Explain macro

This project contains Rust macro `explain` that logs intermediate steps of expression evaluation to the implementation of `std::fmt::Write` or `std::io::Write`.

This macroses takes 2 arguments: formatter and expression.

There are also several helping macroses:
* `explain_stdout` uses standard output as target `Write` implementation
* `explain_stderr` uses standard error output as target `Write` implementation
* `explain_string` redirects logging to the string. 

The returning value for all macroses but `explain_string` is the value of the original expression.
For macros `explain_string` it is a tuple of logged `String` and the original value.

## Project structure

The project contains 3 crates:
* `intermediate_expr_macro` is the main library crate that exposes the public API.
* `helper` is a library crate that was created as a workaround for limitation of exporting procedural macroses. `intermediate_expr_macro` depends on it.
* `example` is a binary crate with several runnable examples in `main` function and with the large test module for the macroses.

## Examples

Macroses usage:
* `explain!(f, -(2 * 5 + 3) * (7 + 6))` where `f` is a formatter
* `explain_stdout!(-(2 * 5 + 3) * (7 + 6))`
* `explain_stderr!(-(2 * 5 + 3) * (7 + 6))`
* `explain_string!(-(2 * 5 + 3) * (7 + 6))`

### Sample outputs
* Arithmetic expression
  ```rust
  let res = explain_string!(-(2 * 5 + 3) * (7 + 6));
  ```
  Output:
  ```markdown
  1. 2 * 5 => 10
  2. 2 * 5 + 3 => 10 + 3 => 13
  3. -(2 * 5 + 3) => -(13) => -13
  4. 7 + 6 => 13
  5. -(2 * 5 + 3) * (7 + 6) => -13 * 13 => -169
  ```
* Multi-line expression
  ```rust
  let res = explain_string!(String::from("str1\nstr2") + "str3");
  ```
  Output:
  ```markdown
  1. String::from("str1\nstr2") => str1
                                   str2
  2. String::from("str1\nstr2") + "str3" => str1 + str3 => str1
                                            str2           str2str3
  ```
* Mutating expression
  ```rust
  let mut y = 5;
  let res = explain_string!(y += 2+8);
  ```
  Output:
  ```markdown
  1. 2 + 8 => 10
  2. (y += 2 + 8) => (y/* = 5 */ += 10) => 15
  ```
* Mutating via reference
  ```rust
  let mut y = 5;
  let res = explain_string!({
      let m: &mut i32 = &mut y;
      *m += 2+8;
  });
  ```
  Output:
  ```markdown
  {
      1. 2 + 8 => 10
      2. (*m += 2 + 8) => (*m/* = 5 */ += 10) => 15
  }
  ```
* Function
  ```rust
  let res = explain_string!(second::<i32>(2 + 3, 4 + 5));
  ```
  Output:
  ```markdown
  1. 2 + 3 => 5
  2. 4 + 5 => 9
  3. second::<i32>(2 + 3, 4 + 5) => second::<i32>(5, 9) => 5
  ```
* Block
  ```rust
  let res = explain_string!({{ 2 + 2 }});
  ```
  Output:
  ```markdown
  {
      {
          1. 2 + 2 => 4
      }
  }
  ```
* While
  ```rust
  let res = explain_string!(while 2 + 3 == 5 { 2 + 2; break });
  ```
  Output:
  ```markdown
  while 2 + 3 == 5 {
      1. 2 + 3 => 5
      2. 2 + 3 == 5 => 5 == 5 => true
      3. 2 + 2 => 4
  }
  ```
* For
  ```rust
  let res = explain_string!(for i in 0..3 { 2 + i; });
  ```
  Output:
  ```markdown
  for i in 0..3 {
      i = 0:
      1. 2 + i => 2
      i = 1:
      1. 2 + i => 3
      i = 2:
      1. 2 + i => 4
  }
  ```
* If
  ```rust
  let res = explain_string!(if 2 == 3 && (S::<i32>(32) as S<i32>).f() { 2 + 2 } else { 3 + 3 });
  ```
  Output:
  ```markdown
  if 2 == 3 && (S::<i32>(32) as S<i32>).f() {
      1. 2 == 3 => false
      5. 2 == 3 && (S::<i32>(32) as S<i32>).f() => false && ... => false
      else:
      7. 3 + 3 => 6
  }
  ```
* Match
  ```rust
  let res = explain_string!(match ((2 + 2)) {
      5 => { 10 + 10 },
      4 => { 10 - 10 },
      _ => { 10 * 10 }
  });
  ```
  Output:
  ```markdown
  match ((2 + 2)) {
      1. 2 + 2 => 4
      pattern 4:
      3. 10 - 10 => 0
  }
  ```
* Method
  ```rust
  let res = explain_string!(S(2).g::<i64>());
  ```
  Output:
  ```markdown
  1. S(2) => S
  2. S(2).g::<i64>() => S.g::<i64>() => true
  ```
* Unicode
  ```rust
  let res = explain_string!(String::from("ç"));
  ```
  Output:
  ```markdown
  1. String::from("ç") => ç
  ```

Other samples can be found in tests of the binary crate `examples`.
  
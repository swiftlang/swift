// RUN: %target-typecheck-verify-swift

// https://bugs.swift.org/browse/SR-1461

class X {}
func foo() {}

let a:[X]()  // expected-error{{unexpected initializer in pattern; did you mean to use '='?}} {{6-7= = }}
let b: [X]()  // expected-error{{unexpected initializer in pattern; did you mean to use '='?}} {{6-7= =}}
let c :[X]()  // expected-error{{unexpected initializer in pattern; did you mean to use '='?}} {{7-8== }}
let d : [X]()  // expected-error{{unexpected initializer in pattern; did you mean to use '='?}} {{7-8==}}

let e: X(), ee: Int  // expected-error{{unexpected initializer in pattern; did you mean to use '='?}} {{6-7= =}}

let f:/*comment*/[X]()  // expected-error{{unexpected initializer in pattern; did you mean to use '='?}} {{6-7= = }}

var _1 = 1, _2 = 2

// paren follows the type, but it's part of a separate (valid) expression
let ff: X
(_1, _2) = (_2, _1)
let fff: X
 (_1, _2) = (_2, _1)

let g: X(x)  // expected-error{{unexpected initializer in pattern; did you mean to use '='?}} {{6-7= =}}
let h: X(x, y)  // expected-error{{unexpected initializer in pattern; did you mean to use '='?}} {{6-7= =}}
let i: X() { foo() }  // expected-error{{unexpected initializer in pattern; did you mean to use '='?}} {{6-7= =}}
let j: X(x) { foo() }  // expected-error{{unexpected initializer in pattern; did you mean to use '='?}} {{6-7= =}}
let k: X(x, y) { foo() }  // expected-error{{unexpected initializer in pattern; did you mean to use '='?}} {{6-7= =}}

func nonTopLevel() {
  let a:[X]()   // expected-error{{unexpected initializer in pattern; did you mean to use '='?}} {{8-9= = }}
  let i: X() { foo() }  // expected-error{{unexpected initializer in pattern; did you mean to use '='?}} {{8-9= =}}
  let j: X(x) { foo() }  // expected-error{{unexpected initializer in pattern; did you mean to use '='?}} {{8-9= =}}
  let k: X(x, y) { foo() }  // expected-error{{unexpected initializer in pattern; did you mean to use '='?}} {{8-9= =}}
  _ = (a, i, j, k)
}

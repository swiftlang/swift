// RUN: %target-typecheck-verify-swift

// https://github.com/apple/swift/issues/44070

class X {}
func foo() {}

let a:[X]()  // expected-cparser-error{{unexpected initializer in pattern; did you mean to use '='?}} {{6-7= = }} expected-astgen-error {{unexpected initializer in pattern; did you mean to use '='?}}{{7-7= = }}{{6-7=}}
let b: [X]()  // expected-cparser-error{{unexpected initializer in pattern; did you mean to use '='?}} {{6-7= =}} expected-astgen-error {{unexpected initializer in pattern; did you mean to use '='?}}{{8-8= = }}{{6-8=}}
let c :[X]()  // expected-cparser-error{{unexpected initializer in pattern; did you mean to use '='?}} {{7-8== }} expected-astgen-error {{unexpected initializer in pattern; did you mean to use '='?}}{{8-8== }}{{7-8=}}
let d : [X]()  // expected-cparser-error{{unexpected initializer in pattern; did you mean to use '='?}} {{7-8==}} expected-astgen-error {{unexpected initializer in pattern; did you mean to use '='?}}{{9-9== }}{{7-9=}}

let e: X(), ee: Int  // expected-cparser-error{{unexpected initializer in pattern; did you mean to use '='?}} {{6-7= =}} expected-astgen-error {{unexpected initializer in pattern; did you mean to use '='?}}{{8-8= = }}{{6-8=}}

let f:/*comment*/[X]()  // expected-cparser-error{{unexpected initializer in pattern; did you mean to use '='?}} {{6-7= = }} expected-astgen-error {{unexpected initializer in pattern; did you mean to use '='?}}{{18-18= =/*comment*/}}{{6-18=}}

var _1 = 1, _2 = 2

// paren follows the type, but it's part of a separate (valid) expression
let ff: X
(_1, _2) = (_2, _1)
let fff: X
 (_1, _2) = (_2, _1)

let g: X(x)  // expected-cparser-error{{unexpected initializer in pattern; did you mean to use '='?}} {{6-7= =}} expected-astgen-error {{unexpected initializer in pattern; did you mean to use '='?}}{{8-8= = }}{{6-8=}} expected-astgen-error {{cannot find 'x' in scope}}
let h: X(x, y)  // expected-cparser-error{{unexpected initializer in pattern; did you mean to use '='?}} {{6-7= =}} expected-astgen-error {{unexpected initializer in pattern; did you mean to use '='?}}{{8-8= = }}{{6-8=}} expected-astgen-error {{cannot find 'x' in scope}} expected-astgen-error {{cannot find 'y' in scope}}
let i: X() { foo() }  // expected-cparser-error{{unexpected initializer in pattern; did you mean to use '='?}} {{6-7= =}} expected-astgen-error {{unexpected initializer in pattern; did you mean to use '='?}}{{8-8= = }}{{6-8=}} expected-astgen-error {{extra trailing closure passed in call}}
let j: X(x) { foo() }  // expected-cparser-error{{unexpected initializer in pattern; did you mean to use '='?}} {{6-7= =}} expected-astgen-error {{unexpected initializer in pattern; did you mean to use '='?}}{{8-8= = }}{{6-8=}} expected-astgen-error {{argument passed to call that takes no arguments}} expected-astgen-error {{cannot find 'x' in scope}}
let k: X(x, y) { foo() }  // expected-cparser-error{{unexpected initializer in pattern; did you mean to use '='?}} {{6-7= =}} expected-astgen-error {{unexpected initializer in pattern; did you mean to use '='?}}{{8-8= = }}{{6-8=}} expected-astgen-error {{argument passed to call that takes no arguments}} expected-astgen-error {{cannot find 'x' in scope}} expected-astgen-error {{cannot find 'y' in scope}}

func nonTopLevel() {
  let a:[X]()   // expected-cparser-error{{unexpected initializer in pattern; did you mean to use '='?}} {{8-9= = }} expected-astgen-error {{unexpected initializer in pattern; did you mean to use '='?}}{{9-9= = }}{{8-9=}}
  let i: X() { foo() }  // expected-cparser-error{{unexpected initializer in pattern; did you mean to use '='?}} {{8-9= =}} expected-astgen-error {{unexpected initializer in pattern; did you mean to use '='?}}{{10-10= = }}{{8-10=}} expected-astgen-error {{extra trailing closure passed in call}}
  let j: X(x) { foo() }  // expected-cparser-error{{unexpected initializer in pattern; did you mean to use '='?}} {{8-9= =}} expected-astgen-error {{unexpected initializer in pattern; did you mean to use '='?}}{{10-10= = }}{{8-10=}} expected-astgen-error {{argument passed to call that takes no arguments}} expected-astgen-error {{cannot find 'x' in scope}}
  let k: X(x, y) { foo() }  // expected-cparser-error{{unexpected initializer in pattern; did you mean to use '='?}} {{8-9= =}} expected-astgen-error {{unexpected initializer in pattern; did you mean to use '='?}}{{10-10= = }}{{8-10=}} expected-astgen-error {{argument passed to call that takes no arguments}} expected-astgen-error {{cannot find 'x' in scope}} expected-astgen-error {{cannot find 'y' in scope}}
  _ = (a, i, j, k)
}

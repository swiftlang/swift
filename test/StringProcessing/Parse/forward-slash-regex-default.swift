// RUN: %target-typecheck-verify-swift

let _ = /x/ // expected-error {{regex literal requires '-enable-experimental-string-processing'}}

prefix operator /
prefix operator ^/

let x = 0
_ = /x
// expected-error@-1 {{'/' is not a prefix unary operator}}

_ = (/x, /x)
// expected-error@-1 {{regex literal requires '-enable-experimental-string-processing'}}
// expected-error@-2 {{expected ',' separator}}

_ = ((/x), /x)
// expected-error@-1 2{{'/' is not a prefix unary operator}}

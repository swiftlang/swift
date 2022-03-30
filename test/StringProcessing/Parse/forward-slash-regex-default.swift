// RUN: %target-typecheck-verify-swift

let _ = /x/ // expected-error {{regex literal requires '-enable-experimental-string-processing'}}

prefix operator /  // expected-error {{prefix slash not allowed}}
prefix operator ^/ // expected-error {{prefix slash not allowed}}

_ = /x
// expected-error@-1 {{prefix slash not allowed}}
// expected-error@-2 {{'/' is not a prefix unary operator}}
// expected-error@-3 {{cannot find 'x' in scope}}

_ = !/x/ // expected-error {{regex literal requires '-enable-experimental-string-processing'}}

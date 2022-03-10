// RUN: %target-typecheck-verify-swift -enable-experimental-string-processing
// REQUIRES: swift_in_compiler

_ = re'(' // expected-error {{expected ')'}}

// FIXME: Should be 'group openings'
_ = re')' // expected-error {{closing ')' does not balance any groups openings}}

let s = #/\\/''/ // expected-error {{unterminated regex literal}}
_ = #|\| // expected-error {{unterminated regex literal}}
_ = #// // expected-error {{unterminated regex literal}}
_ = re'x // expected-error {{unterminated regex literal}}

// expected-error@+1 {{unterminated regex literal}}
var unterminated = #/xy

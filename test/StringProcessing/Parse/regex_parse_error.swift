// RUN: %target-typecheck-verify-swift -enable-experimental-string-processing
// REQUIRES: swift_in_compiler

_ = re'(' // expected-error {{expected ')'}}

// FIXME: Should be 'group openings'
_ = re')' // expected-error {{closing ')' does not balance any groups openings}}

_ = #/\\/''/ // expected-error {{unterminated regex literal}}
_ = #|\| // expected-error {{unterminated regex literal}}
_ = #// // expected-error {{unterminated regex literal}}
_ = re'x // expected-error {{unterminated regex literal}}

_ = #/xy // expected-error {{unterminated regex literal}}

_ = re'(?' // expected-error {{expected group specifier}}

_ = re'(?'' // expected-error {{unterminated regex literal}}
// expected-error@-1 {{expected group name}}

_ = re'(?'abc' // expected-error {{unterminated regex literal}}
// expected-error@-1 {{expected ')'}}

// TODO: Maybe change "unterminated string literal" to "unterminated single quote"?
_ = re'(?'abc ' // expected-error {{unterminated string literal}}
// expected-error@-1 {{expected group specifier}}
// expected-error@-2 {{consecutive statements on a line must be separated by ';'}}

_ = re'(?'a // expected-error {{expected group specifier}}
// expected-error@-1 {{cannot find 'a' in scope}}
// expected-error@-2 {{consecutive statements on a line must be separated by ';'}}

_ = re'\(?'abc' // expected-error {{unterminated string literal}}
// expected-error@-1 {{consecutive statements on a line must be separated by ';'}}

 _ = re'\
 '
// expected-error@-2 {{unterminated regex literal}}
// expected-error@-3 {{expected escape sequence}}
// expected-error@-3 {{unterminated string literal}}

func foo<T>(_ x: T, _ y: T) {}
foo(re'(?', re'abc') // expected-error {{expected group specifier}}
foo(re'(?C', re'abc') // expected-error {{expected ')'}}

foo(re'(?'', re'abc') // expected-error {{expected group name}}
// expected-error@-1 {{unterminated string literal}}
// expected-error@-2 {{expected ',' separator}}

// RUN: %target-parse-verify-swift

// expected-error @+4 {{type annotation missing in pattern}}
// expected-error @+3 {{use of unresolved operator '⁚'}}
// expected-error @+2 {{operator with postfix spacing cannot start a subexpression}}
// expected-error @+1 {{consecutive statements on a line must be separated by ';'}}
let number⁚ Int // expected-note {{operator '⁚' contains possibly confused characters; did you mean to use ':'?}} {{11-14=:}}

// expected-error @+3 2 {{invalid character in source file}}
// expected-error @+2 {{consecutive statements on a line must be separated by ';'}}
// expected-note @+1 2 {{unicode character '‒' looks similar to '-'; did you mean to use '-'?}} {{3-6=-}}
5 ‒ 5

// expected-error @+2 {{use of unresolved identifier 'ꝸꝸꝸ'}}
// expected-error @+1 2 {{expected ',' separator}}
if (true ꝸꝸꝸ false) {} // expected-note {{identifier 'ꝸꝸꝸ' contains possibly confused characters; did you mean to use '&&&'?}} {{10-19=&&&}}


// RUN: %target-typecheck-verify-swift

// expected-error @+4 {{type annotation missing in pattern}}
// expected-error @+3 {{use of unresolved operator '⁚'}}
// expected-error @+2 {{operator with postfix spacing cannot start a subexpression}}
// expected-error @+1 {{consecutive statements on a line must be separated by ';'}}
let number⁚ Int // expected-note {{operator '⁚' contains possibly confused characters; did you mean to use ':'?}} {{11-14=:}}

// expected-warning @+3 2 {{integer literal is unused}}
// expected-error @+2 {{invalid character in source file}}
// expected-error @+1 {{consecutive statements on a line must be separated by ';'}}
5 ‒ 5 // expected-note {{unicode character '‒' looks similar to '-'; did you mean to use '-'?}} {{3-6=-}}

// expected-error @+2 {{use of unresolved identifier 'ꝸꝸꝸ'}}
// expected-error @+1 {{expected ',' separator}}
if (true ꝸꝸꝸ false) {} // expected-note {{identifier 'ꝸꝸꝸ' contains possibly confused characters; did you mean to use '&&&'?}} {{10-19=&&&}}

// expected-error @+4 {{invalid character in source file}}
// expected-error @+3 {{expected ',' separator}}
// expected-error @+2 {{binary operator '==' cannot be applied to operands of type '(Int, Int)' and 'Int'}}
// expected-note @+1 {{expected an argument list of type '(Int, Int)'}}
if (5 ‒ 5) == 0 {} // expected-note {{unicode character '‒' looks similar to '-'; did you mean to use '-'?}} {{7-10=-}}

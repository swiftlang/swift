// RUN: %target-typecheck-verify-swift -verify-ignore-unrelated

// expected-error @+4 {{type annotation missing in pattern}}
// expected-cparser-error @+3 {{cannot find operator '⁚' in scope}}
// expected-cparser-error @+2 {{operator with postfix spacing cannot start a subexpression}}
// expected-cparser-error @+1 {{consecutive statements on a line must be separated by ';'}}
let number⁚ Int // expected-cparser-note {{operator '⁚' (Two Dot Punctuation) looks similar to ':' (Colon); did you mean ':' (Colon)?}} {{11-14=:}} expected-astgen-error {{unexpected code '⁚ Int' in source file}}

// expected-warning @+3 2 {{integer literal is unused}}
// expected-error @+2 {{invalid character in source file}}
// expected-cparser-error @+1 {{consecutive statements on a line must be separated by ';'}}
5 ‒ 5 // expected-cparser-note {{unicode character '‒' (Figure Dash) looks similar to '-' (Hyphen Minus); did you mean to use '-' (Hyphen Minus)?}} {{3-6=-}}

// expected-cparser-error @+3 {{cannot convert value of type '(Bool, _)' to expected condition type 'Bool'}}
// expected-cparser-error @+2 {{cannot find 'ꝸꝸꝸ' in scope}}
// expected-cparser-error @+1 {{expected ',' separator}}
if (true ꝸꝸꝸ false) {} // expected-cparser-note {{identifier 'ꝸꝸꝸ' contains possibly confused characters; did you mean to use '&&&'?}} {{10-19=&&&}} expected-astgen-error {{unexpected code 'ꝸꝸꝸ false' in tuple}}

// expected-error @+3 {{invalid character in source file}}
// expected-cparser-error @+2 {{expected ',' separator}}
// expected-cparser-error @+1 {{binary operator '==' cannot be applied to operands of type '(Int, Int)' and 'Int'}}
if (5 ‒ 5) == 0 {} // expected-cparser-note {{unicode character '‒' (Figure Dash) looks similar to '-' (Hyphen Minus); did you mean to use '-' (Hyphen Minus)?}} {{7-10=-}} expected-astgen-error {{unexpected code '5' in tuple}}

// GREEK QUESTION MARK (which looks like a semicolon) 
print("A"); print("B")
// expected-cparser-error@-1 2{{consecutive statements on a line must be separated by ';'}} expected-astgen-error@-1 2{{consecutive statements on a line must be separated by newline or ';'}}
// expected-error@-2 {{cannot find ';' in scope}}
// expected-note@-3 {{identifier ';' (Greek Question Mark) looks similar to ';' (Semicolon); did you mean ';' (Semicolon)?}} {{11-13=;}}

// RUN: %target-typecheck-verify-swift -parse-as-library
// RUN: %target-typecheck-verify-swift -parse-as-library -enable-astscope-lookup

let x = 42
x + x; // expected-error {{expressions are not allowed at the top level}} expected-warning {{result of operator '+' is unused}}
x + x; // expected-error {{expressions are not allowed at the top level}} expected-warning {{result of operator '+' is unused}}
// Make sure we don't crash on closures at the top level
({ }) // expected-error {{expressions are not allowed at the top level}} expected-error{{expression resolves to an unused function}}
({ 5 }()) // expected-error {{expressions are not allowed at the top level}}
// expected-warning @-1 {{result of call to closure returning 'Int' is unused}}


// expected-error @+3 {{expected 'in' after for-each pattern}}
// expected-error @+2 {{expected Sequence expression for for-each loop}}
// expected-error @+1 {{expected '{' to start the body of for-each loop}}
for i

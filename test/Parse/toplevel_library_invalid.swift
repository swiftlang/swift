// RUN: %target-typecheck-verify-swift -parse-as-library
// RUN: %target-typecheck-verify-swift -parse-as-library -enable-astscope-lookup

let x = 42 // expected-note{{did you mean 'x'?}}
x + x; // expected-error {{expressions are not allowed at the top level}} expected-warning {{result of operator '+' is unused}}
x + x; // expected-error {{expressions are not allowed at the top level}} expected-warning {{result of operator '+' is unused}}
// Make sure we don't crash on closures at the top level
({ }) // expected-error {{expressions are not allowed at the top level}} expected-error{{expression resolves to an unused function}}
({ 5 }()) // expected-error {{expressions are not allowed at the top level}}
// expected-warning @-1 {{result of call is unused}}


// FIXME: Too many errors for this.
for i // expected-error 2 {{expected ';' in 'for' statement}} 
      // expected-error @-1{{use of unresolved identifier 'i'}}
      // expected-error @+3{{expected '{' in 'for' statement}}
      // expected-error @+2{{expected condition in 'for' statement}}
      // expected-error @+1{{expected expression}}

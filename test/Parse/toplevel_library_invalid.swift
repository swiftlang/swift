// RUN: %target-parse-verify-swift -parse-as-library

let x = 42
x + x; // expected-error {{expressions are not allowed at the top level}}
x + x; // expected-error {{expressions are not allowed at the top level}}
// Make sure we don't crash on closures at the top level
({ }) // expected-error {{expressions are not allowed at the top level}} expected-error{{type of expression is ambiguous without more context}}
({ 5 }()) // expected-error {{expressions are not allowed at the top level}}

// FIXME: Too many errors for this.
for i // expected-error 2 {{expected ';' in 'for' statement}} 
      // expected-error @-1{{use of unresolved identifier 'i'}}
      // expected-error @+3{{expected '{' in 'for' statement}}
      // expected-error @+2{{expected condition in 'for' statement}}
      // expected-error @+1{{expected expression}}

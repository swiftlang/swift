// RUN: %target-typecheck-verify-swift 

// https://github.com/apple/swift/issues/63753

if 0 {} // expected-error{{integer literal value '0' cannot be used as a boolean; did you mean 'false'?}} {{4-5=false}}
if (0) {} // expected-error{{integer literal value '0' cannot be used as a boolean; did you mean 'false'?}} {{5-6=false}}
if 1 {} // expected-error{{integer literal value '1' cannot be used as a boolean; did you mean 'true'?}} {{4-5=true}}
if (1) {} // expected-error{{integer literal value '1' cannot be used as a boolean; did you mean 'true'?}} {{5-6=true}}
if 12 {} // expected-error{{cannot convert value of type 'Int' to expected condition type 'Bool'}}
if (12) {} // expected-error{{cannot convert value of type 'Int' to expected condition type 'Bool'}}

if 0? {} // expected-error {{cannot use optional chaining on non-optional value of type 'Int'}}
// expected-error@-1{{cannot convert value of type 'Int?' to expected condition type 'Bool'}}
if (0?) {} // expected-error {{cannot use optional chaining on non-optional value of type 'Int'}}
// expected-error@-1{{cannot convert value of type 'Int?' to expected condition type 'Bool'}}
if 0?? {} // expected-error 2{{cannot use optional chaining on non-optional value of type 'Int'}}
// expected-error@-1{{cannot convert value of type 'Int?' to expected condition type 'Bool'}}

let _ = 0? as Int? // expected-error {{cannot use optional chaining on non-optional value of type 'Int'}}
let _ = nil? as Int? // expected-error {{'?' must be followed by a call, member lookup, or subscript}}
let _ = ""? as String? // expected-error {{cannot use optional chaining on non-optional value of type 'String'}}

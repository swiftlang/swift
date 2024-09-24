// RUN: %target-typecheck-verify-swift

let a: 123 // expected-error {{integer unexpectedly used in a type position}}

let b: -123 // expected-error {{integer unexpectedly used in a type position}}

let c: -Int // expected-error {{expected type}}
            // expected-error@-1 {{consecutive statements on a line must be separated by ';'}}
            // expected-error@-2 {{unary operator '-' cannot be applied to an operand of type 'Int.Type'}}

struct Generic<T> {} // expected-note {{'T' declared as parameter to type 'Generic'}}
                     // expected-note@-1 {{'T' declared as parameter to type 'Generic'}}

extension Generic where T == 123 {} // expected-error {{cannot constrain type parameter 'T' to be integer '123'}}

extension Generic where T == -123 {} // expected-error {{cannot constrain type parameter 'T' to be integer '-123'}}

extension Generic where T == -Int {} // expected-error {{expected type}}
                                     // expected-error@-1 {{expected '{' in extension}}

let d = Generic<123>.self // expected-error {{integer unexpectedly used in a type position}}

// FIXME: This should at least be parsable...?
let e = Generic<-123>.self // expected-error {{generic parameter 'T' could not be inferred}}
                           // expected-error@-1 {{missing whitespace between '<' and '-' operators}}
                           // expected-error@-2 {{'>' is not a postfix unary operator}}
                           // expected-note@-3 {{explicitly specify the generic arguments to fix this issue}}

let f = Generic<-Int>.self // expected-error {{generic parameter 'T' could not be inferred}}
                           // expected-error@-1 {{missing whitespace between '<' and '-' operators}}
                           // expected-error@-2 {{'>' is not a postfix unary operator}}
                           // expected-note@-3 {{explicitly specify the generic arguments to fix this issue}}

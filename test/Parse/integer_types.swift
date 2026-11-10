// RUN: %target-typecheck-verify-swift

let a: 123 // expected-cparser-error {{consecutive statements on a line must be separated by ';'}} expected-astgen-error {{expected '=' in variable}}
           // expected-error@-1 {{expected type}}
           // expected-cparser-warning@-2 {{integer literal is unused}}

let b: -123 // expected-cparser-error {{consecutive statements on a line must be separated by ';'}} expected-astgen-error {{expected '=' in variable}}
           // expected-error@-1 {{expected type}}
           // expected-cparser-warning@-2 {{integer literal is unused}}

let c: -Int // expected-error {{expected type}}
            // expected-cparser-error@-1 {{consecutive statements on a line must be separated by ';'}}
            // expected-cparser-error@-2 {{unary operator '-' cannot be applied to an operand of type 'Int.Type'}} expected-astgen-error@-2 {{expected '=' in variable}}

struct Generic<T> {}
extension Generic where T == 123 {} // expected-error {{cannot constrain type parameter 'T' to be integer '123'}}

extension Generic where T == -123 {} // expected-error {{cannot constrain type parameter 'T' to be integer '-123'}}

extension Generic where T == -Int {} // expected-cparser-error {{expected type}} expected-astgen-error {{unexpected code '-Int' in extension}}
                                     // expected-cparser-error@-1 {{expected '{' in extension}} expected-astgen-error@-1 {{expected right-hand type in same type requirement}}

let d = Generic<123>.self // expected-error {{cannot use value type '123' for generic argument 'T'}}

// FIXME: This should at least be parsable...?
let e = Generic<-123>.self // expected-cparser-error {{missing whitespace between '<' and '-' operators}}
                           // expected-cparser-error@-1 {{'>' is not a postfix unary operator}} expected-astgen-error@-1 {{cannot use value type '-123' for generic argument 'T'}}

let f = Generic<-Int>.self // expected-error {{missing whitespace between '<' and '-' operators}}
                           // expected-error@-1 {{'>' is not a postfix unary operator}}

let g: 123.Type // expected-cparser-error {{consecutive statements on a line must be separated by ';'}} expected-astgen-error {{expected '=' in variable}}
                // expected-error@-1 {{expected type}}
                // expected-error@-2 {{value of type 'Int' has no member 'Type'}}

let h: 123.Protocol // expected-cparser-error {{consecutive statements on a line must be separated by ';'}} expected-astgen-error {{expected '=' in variable}}
                    // expected-error@-1 {{expected type}}
                    // expected-error@-2 {{value of type 'Int' has no member 'Protocol'}}

let i: 123? // expected-cparser-error {{consecutive statements on a line must be separated by ';'}} expected-astgen-error {{expected '=' in variable}}
            // expected-error@-1 {{expected type}}
            // expected-error@-2 {{cannot use optional chaining on non-optional value of type 'Int'}}

let j: 123! // expected-cparser-error {{consecutive statements on a line must be separated by ';'}} expected-astgen-error {{expected '=' in variable}}
            // expected-error@-1 {{expected type}}
            // expected-error@-2 {{cannot force unwrap value of non-optional type 'Int'}}

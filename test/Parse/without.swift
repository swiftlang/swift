// RUN: %target-typecheck-verify-swift

// checks expectations of parsing code with the without operator `~`
// the majority of the positive cases are already covered in another test.

func x<~T>(_ f: T) {} // expected-error {{expected an identifier to name generic parameter}}
                      // expected-error@-1 {{cannot find type 'T' in scope}}

func someFn() {}
let z = ~someFn.self // expected-error {{unary operator '~' cannot be applied to an operand of type '() -> ()'}}

struct X: ~~Copyable {} // expected-error {{expected type}}
                        // expected-error@-1 {{expected '{' in struct}}

extension Array where Element: ~~Copyable {} // expected-error {{expected type}}
                                             // expected-error@-1 {{expected '{' in extension}}

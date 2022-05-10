// RUN: %target-typecheck-verify-swift

postfix operator `!`
// expected-error@-1 {{cannot declare a custom postfix '!' operator}}
// expected-error@-2 {{postfix operator names starting with '?' or '!' are disallowed to avoid collisions with built-in unwrapping operators}}

postfix operator `?`
// expected-error@-1 {{cannot declare a custom postfix '?' operator}}
// expected-error@-2 {{postfix operator names starting with '?' or '!' are disallowed to avoid collisions with built-in unwrapping operators}}

postfix operator `?|`
// expected-error@-1 {{postfix operator names starting with '?' or '!' are disallowed to avoid collisions with built-in unwrapping operators}}

let _ = 1 `+` 2

func foo(_ x: Int?) -> Int {
  // TODO: This gets lexed separately, so gets turned into a non-special operator.
  // Should we support this?
  x`!` // expected-error {{'!' is not a postfix unary operator}}
}

func bar(_ x: Bool) -> Bool {
  `!`x
}

`+ // expected-error {{expected expression}}

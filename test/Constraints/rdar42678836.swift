// RUN: %target-typecheck-verify-swift

func foo(chr: Character) -> String {
  return String(repeating: String(chr)) // expected-error {{incorrect argument label in call (have 'repeating:', expected 'stringLiteral:')}}
}

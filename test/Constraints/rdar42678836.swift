// RUN: %target-typecheck-verify-swift

func foo(chr: Character) -> String {
  return String(repeating: String(chr)) // expected-error {{no exact matches in call to initializer}}
  // expected-note@-1 {{candidate has partially matching parameter list (repeating: String, count: Int)}}
}

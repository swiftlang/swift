// RUN: %target-typecheck-verify-swift

func unavailableModulo(description: String = "<rdar://problem/27855641>") {
  _ = 42.0 % 2 // expected-error {{For floating point numbers use truncatingRemainder}}
  var f: Float = 42.0
  f %= 2 // expected-error {{For floating point numbers use formTruncatingRemainder}}
}

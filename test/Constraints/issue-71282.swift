// RUN: %target-typecheck-verify-swift

// https://github.com/apple/swift/issues/71282

enum E {
  case e
}

// Make sure we don't crash.
func foo(_ x: E) {
  return if .random() {
    ()
    switch x {
    case .e:
      ()
    }
  } else { // expected-error {{non-expression branch of 'if' expression may only end with a 'throw'}}
    ()
  }
}

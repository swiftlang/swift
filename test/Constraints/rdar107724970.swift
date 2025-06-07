// RUN: %target-typecheck-verify-swift

// rdar://107724970 – Make sure we don't crash.
enum E {
  case e(Int)
}
func foo(_ x: E) {
  // https://github.com/apple/swift/issues/65062
  let fn = {
    switch x {
    case E.e(_, _):
      // expected-error@-1 {{tuple pattern has the wrong length for tuple type 'Int'}}
      break
    }
  }
}

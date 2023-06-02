// RUN: %target-typecheck-verify-swift

// rdar://107709341 – Make sure we don't crash.
func foo(_ x: Int) {
  // FIXME: We ought to have a better diagnostic
  let _ = { // expected-error {{unable to infer closure type in the current context}}
    switch x {
    case Optional<Int>.some(x):
      break
    default:
      break
    }
  }
}

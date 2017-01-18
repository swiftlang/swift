// RUN: %target-typecheck-verify-swift

struct S {
  init(a:Bool) {
    return
  }

  init(b:Bool) {
    return 1 // expected-error {{'nil' is the only return value permitted in an initializer}}
  }
}


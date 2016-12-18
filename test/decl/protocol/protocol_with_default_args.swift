// RUN: %target-typecheck-verify-swift

protocol P {
  func foo(_ truth: Bool = false) // expected-error{{default argument not permitted in a protocol method}}
}

struct X : P {
  func foo(_ truth: Bool = false) {
  }
}

protocol Q {
  init(truth: Bool = false) // expected-error{{default argument not permitted in a protocol initializer}}
}

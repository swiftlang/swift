// RUN: %target-typecheck-verify-swift -swift-version 3

struct X {
  func f1(_ i: Int) { }
  mutating func f1(_ f: Float) { }
}

func g0(_: (inout X) -> (Float) -> ()) {}

g0(X.f1) // expected-warning{{partial application of 'mutating' method}}

// RUN: %target-typecheck-verify-swift -swift-version 3

struct X {
  func f1(_ i: Int) { }
  mutating func f1(_ f: Float) { }
}

func g0(_: (inout X) -> (Float) -> ()) {}

// This becomes an error in Swift 4 mode -- probably a bug
g0(X.f1)

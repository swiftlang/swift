// RUN: %target-swift-frontend -emit-silgen %s

// This is a SILGen test to ensure we can completely check these conformances
// and build valid AST.

protocol P {
  associatedtype T : Q = S
  typealias Y = T.X

  func foo(_: T.X)
}

protocol Q {
  associatedtype X
}

struct S : Q {
  typealias X = ()
}

struct R : P {
  let x: Y? = nil
  func foo(_: Y) {}
}

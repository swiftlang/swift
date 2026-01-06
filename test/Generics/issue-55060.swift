// RUN: %target-typecheck-verify-swift

protocol E {}

protocol P {
  associatedtype V: E
}

protocol Q {
  associatedtype G: P
}

protocol R {
  associatedtype G
  associatedtype M: Q where M.G == G
}

struct A<I: E>: P {
  typealias V = I
  func f<N: R>(_: N) where N.G == Self {}
}

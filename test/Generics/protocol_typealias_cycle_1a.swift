// RUN: %target-typecheck-verify-swift

protocol P {
  associatedtype X
  associatedtype Y where Y : Q
}

protocol Q {
  associatedtype T
}

struct S: Q {
  typealias T = Int
}

extension P where X == () {
  typealias Y = S
}

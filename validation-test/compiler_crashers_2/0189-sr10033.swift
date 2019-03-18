// RUN: not --crash %target-swift-frontend -typecheck %s

// REQUIRES: asserts

protocol P1 {
  associatedtype A2 : P2 where A2.A1 == Self
}

protocol P2 {
  associatedtype A1 : P1 where A1.A2 == Self
  var property: Int { get }
}

extension P2 {
  var property: Int { return 0 }
}

class C1 : P1 {
  class A2 : P2 {
    typealias A1 = C1
  }
}

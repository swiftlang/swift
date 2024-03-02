// RUN: %target-swift-frontend -emit-ir -verify %s
// RUN: %target-swift-frontend -emit-ir -verify -disable-requirement-machine-concrete-contraction %s

// https://github.com/apple/swift/issues/52436

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
// expected-warning@-1 {{non-final class 'C1' cannot safely conform to protocol 'P1', which requires that 'Self.A2.A1' is exactly equal to 'Self'; this is an error in the Swift 6 language mode}}
  class A2 : P2 {
  // expected-warning@-1 {{non-final class 'C1.A2' cannot safely conform to protocol 'P2', which requires that 'Self.A1.A2' is exactly equal to 'Self'; this is an error in the Swift 6 language mode}}
    typealias A1 = C1
  }
}

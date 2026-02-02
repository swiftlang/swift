// RUN: %target-swift-frontend %s -typecheck

public protocol P1 {
  associatedtype A1
}

public protocol P2 : P1 {
  associatedtype A2 : P1
  var prop1: A2 { get }
}

extension P2 {
  public typealias A1 = A2
}

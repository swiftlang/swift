// RUN: %target-swift-frontend -typecheck -verify %s

// This used to hit a circularity.

public protocol P {}

public struct G<T : P> {}

public typealias A<T : P> = G<T>

public protocol Circle {
  associatedtype X : P
  associatedtype Y where Y == A<X>
}



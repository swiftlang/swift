public protocol P1 {
  associatedtype A: Sendable
}

public protocol P2: P1, Sendable where A == any P2 {}

// RUN: %target-swift-frontend -emit-silgen %s

public struct G<A, B, C>: P {
  public typealias D = G<A, C, B>
  public typealias E = G<A, S, C>
  public typealias F = G<A, B, S>
}

public struct S {}

public protocol P {
  associatedtype A
  associatedtype B
  associatedtype C
  associatedtype D: P where D.A == A, D.D == Self
  associatedtype E: P where E.C == C, E.B == S, E.A == A, E.E == E
  associatedtype F: P where F.C == S, F.B == B, F.A == A, F.F == F
}

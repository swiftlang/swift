// RUN: %target-typecheck-verify-swift
// RUN: %target-swift-frontend -emit-ir %s

public struct G<A : P7> : P2 {}

public protocol P1 {
  associatedtype A
}

public protocol P2: P1 where A: P7 {}

public protocol P3 {
  associatedtype B: P1 where B.A == A
  associatedtype A: P7
}

public protocol P4: P3 where B == G<A> { }

public protocol P5 : P3 {
  associatedtype C: P3 where C.A == A
}

public protocol P6 : P5 where B == G<A>, C: P4 {}

public protocol P7 {
  associatedtype D: P1 where D.A == Self
}

public protocol P8 {
  associatedtype E: P5 where E.C.B: P2
  associatedtype F: P4 where F.A == E.A
}

public protocol P9 : P8 where E.C == F, E: P6 {}

public func callee<T : P1>(_: T.Type) -> T {
  fatalError()
}

public func callee<T : P9>(_: T) {
  _ = callee(T.F.A.D.self)
}

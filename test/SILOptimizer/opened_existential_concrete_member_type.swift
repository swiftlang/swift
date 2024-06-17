// RUN: %target-swift-frontend -emit-ir %s

public protocol P1 {}

public protocol P2<A> {
  associatedtype A
}

public struct S: P1 {}

public struct G<A>: P2 {}

public func callee<T: P2>(_: T) where T.A: P1 {}

@_transparent
public func caller<A: P1>(_ p: any P2<A>) {
  callee(p)
}

public func test(_ a: G<S>, _ b: G<S>) {
  return caller(b)
}


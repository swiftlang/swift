// RUN: %target-swift-frontend -emit-ir %s

public protocol P1 {
  associatedtype A: P3
}

public protocol P2: P1 where A.B == G<C> {
  associatedtype C where C == A.B.C
}

public protocol P3 {
  associatedtype B: P4
}

public protocol P4: P5 {}

public protocol P5 {
  associatedtype C: P6
}

public protocol P6 {
  func foo()
}

public struct G<C: P6>: P4 {}

public func f<T: P2>(_: T, c: T.C) {
  return c.foo()
}

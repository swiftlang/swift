// RUN: %target-swift-frontend -verify -emit-ir %s

public protocol P1 {}

public protocol P2 {
    associatedtype A : P1
    associatedtype B : P2
    func f()
}

public extension P2 where B.A == A {
    func f() {}
}

public class C<B: P2>: P2 {
    public typealias A = B.A
}

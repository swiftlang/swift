// RUN: %target-typecheck-verify-swift

public protocol Q1 {}
public protocol Q2 {}

protocol P1 {
    associatedtype A
    func f<T: Q1>(_: T) -> A
}

protocol P2 {
    associatedtype A
    func f<T: Q2>(_: T) -> A
}

struct S1 {}
struct S2 {}

// Associated type inference isn't smart enough to reason about the
// requirement on `T`, so it considers both overloads of f() as viable
// witnesses. However, they infer the same binding 'A := S2', so
// it's fine.

extension S1: P1 {
    public func f<T: Q1>(_: T) -> S2 {}
}

extension S1: P2 {
    public func f<T: Q2>(_: T) -> S2 {}
}


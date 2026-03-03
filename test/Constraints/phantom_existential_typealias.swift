// RUN: %target-typecheck-verify-swift

// This is an elaborate reduction for a problem where we create a TypeAliasType
// with a concrete underlying type, but type variables in its substitutions.
//
// Specifically, we get something like `G<$1, $2>.B` below, but the underlying
// type of B is just S, it does not depend on its generic arguments.
//
// The solver doesn't simplify the type variables away in this case, so to
// avoid problems the TypeTransform desugars the typealias in this case.

public protocol P {
    associatedtype A
}

public struct Horse<A>: P {}

public struct S {}

@resultBuilder
public enum E<A, B> {
    public static func buildExpression<T: P>(_ t: T) -> T where T.A == A {
        return t
    }

    public static func buildBlock<T: P>(_ t: T) -> T {
        return t
    }
}

public struct G<A, C: P> where C.A == A {
    public typealias B = S

    public init(_: A.Type, @E<A, B> _: () -> C) {}
}

func testHorse() {
    _ = G(S.self) {
        Horse()
    }
}

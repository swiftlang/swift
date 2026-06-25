// RUN: %target-typecheck-verify-swift -solver-scope-threshold=100

protocol P1 {}

extension P1 {
    static func +(lhs: Self, rhs: Self) -> Self { fatalError() }
    static func -(lhs: Self, rhs: Self) -> Self { fatalError() }
    static func *(lhs: Self, rhs: Double) -> Self { fatalError() }
    static func /(lhs: Self, rhs: Double) -> Self { fatalError() }
}

struct S1: P1 {}

protocol P2 {
    associatedtype A
    associatedtype B
}

protocol P3 {
    associatedtype A: P2
}

extension P3 {
    var x: A.B { fatalError() }
    var y: A.B { fatalError() }
}

struct S3<A: P2>: P3 {
    // Note that S3 has two overloads of 'x' and 'y' each, one that
    // returns A.B and one that returns 'S1'.
    var x: S1 { fatalError() }
    var y: S1 { fatalError() }
}

struct S2: P2 {
    typealias A = S3<S2>
    typealias B = Double
}

struct G<A: P2> {
    init(first: A.A, second: A.A) {}
    init(first: (A.B, A.B), second: (A.B, A.B)) {}
}

func test(x: S3<S2>, y: Double, z: Double) {
    // This used to be slow because the solver would struggle with dead ends
    // involving the wrong overloads of 'x.x' and 'x.y'.
    let _ = G<S2>(first: (x.x - y / 2, x.y - z / 2),
                  second: (x.x + y / 2, x.y + z / 2))
}

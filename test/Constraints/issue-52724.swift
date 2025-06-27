// RUN: %target-swift-frontend -typecheck -verify -disable-constraint-solver-performance-hacks %s

// https://github.com/apple/swift/issues/52724

struct A {
    static func * (lhs: A, rhs: A) -> B { return B() }
    static func * (lhs: B, rhs: A) -> B { return B() }
    static func * (lhs: A, rhs: B) -> B { return B() }
}
struct B {}

let (x, y, z) = (A(), A(), A())

let w = A() * A() * A()     // works

// Should all work
let a = x * y * z
let b = x * (y * z)
let c = (x * y) * z
let d = x * (y * z as B)
let e = (x * y as B) * z

// RUN: %target-swift-frontend %s -typecheck -verify
// RUN: %target-swift-frontend %s -emit-ir -o /dev/null

// SR-5601
protocol P1 {
    associatedtype X: P3 where X.Q == Self, X.R == UInt8
    associatedtype Y: P3 where Y.Q == Self, Y.R == UInt16
    // NOTE: Removing either X or Y from P1 (and A) makes the program compile.
}
struct A: P1 {
    typealias X = S<UInt8>
    typealias Y = S<UInt16>
}
protocol P2 { }
protocol P3 : P2 { // NOTE: Removing ": P2 " here makes the program compile.
    associatedtype Q: P1
    associatedtype R
}
struct S<E> : P3 {
    typealias R = E
    typealias Q = A
}



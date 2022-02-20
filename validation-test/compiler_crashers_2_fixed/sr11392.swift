// RUN: %target-swift-frontend -verify -emit-ir %s

protocol P1 {
    associatedtype X
    associatedtype A: P2 where A.X == X
}

protocol P2 {
    associatedtype X
}

struct S {}

extension S {
    struct A: P2 {
        typealias X = Int
    }
}


extension S: P1 {}

print(S.X.self)

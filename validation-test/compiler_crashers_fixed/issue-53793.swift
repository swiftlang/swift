// RUN: %target-swift-frontend -emit-ir %s

// https://github.com/apple/swift/issues/53793

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

let x: Int.Type = S.X.self

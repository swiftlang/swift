// RUN: %target-swift-frontend -verify -emit-ir %s

// Works with experimental associated type inference.
// RUN: %target-swift-frontend -enable-experimental-associated-type-inference -emit-ir %s

// https://github.com/apple/swift/issues/53793

protocol P1 {
    associatedtype X
    // expected-note@-1 {{protocol requires nested type 'X'; add nested type 'X' for conformance}}
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
// expected-error@-1 {{type 'S' does not conform to protocol 'P1'}}

print(S.X.self)

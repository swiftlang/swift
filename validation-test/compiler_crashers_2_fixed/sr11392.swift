// RUN: %target-swift-frontend -verify -emit-ir %s

// Ideally this would type check successfully; we should be able to
// infer that X == Int using the same-type constraint 'A.X == X'.

protocol P1 {
    associatedtype X
    // expected-note@-1 {{protocol requires nested type 'X'; do you want to add it?}}
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

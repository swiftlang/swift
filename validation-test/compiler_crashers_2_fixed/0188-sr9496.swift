// RUN: %target-swift-frontend -typecheck %s

protocol P1 {
    associatedtype A1
}

protocol P2 {
    associatedtype A2
}

struct S1<G1: P1, G2: P1>: P1 where G1.A1 == G2.A1 {
    typealias A1 = G1.A1
}

struct S2<G1: P1, G2: P2>: P2 where G1.A1 == G2.A2 {
    typealias A2 = G2.A2
}

struct S3<G1: P1, G2: P2> where G1.A1 == G2.A2 {
    func f<G: P1>(_: G) -> S3<S1<G, G1>, S2<G, G2>> {
        fatalError()
    }
}

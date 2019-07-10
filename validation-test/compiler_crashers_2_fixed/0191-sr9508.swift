// RUN: %target-swift-frontend -typecheck %s

protocol P {
    associatedtype A
}

struct S1: P {
    typealias A = Int
}

struct S2<G: P>: P {
    typealias A = G.A
}

struct S3<G: P> {
}

extension S3 where G == S2<S1> {
    typealias B = G.A
}

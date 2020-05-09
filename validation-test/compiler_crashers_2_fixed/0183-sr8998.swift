// RUN: not %target-swift-frontend -emit-ir %s

protocol P {
    associatedtype A: Q where A.B == Self
}

protocol Q {
    associatedtype B: P where B.A == Self
}

struct S1<T>: P where T: Q { }

struct S2: Q {
    typealias B = S1<S2>
}

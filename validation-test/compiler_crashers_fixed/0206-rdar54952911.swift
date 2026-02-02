// RUN: %target-swift-frontend -emit-silgen %s
// FIXME: Get the above to pass with -emit-ir too.

public protocol P1 {
    associatedtype A1: P3 where A1.A4.A3: P6
}

public protocol P12 : P1 where A1: P2 {}

public protocol P2 : P3 where A3 == S3<A2>, A4: P4 {}

public protocol P4 : P3 where A3 == S2<A2>, A4: P5 {}

public protocol P5: P9 where A3 == S1<A2> {}

public protocol P6: P11 where A2: P7 {}

public protocol P7: P8 {}

public protocol P8 {}

public protocol P11 {
	associatedtype A2 : P8
}

public struct S1<A2 : P8> : P11 {}

public struct S2<A2 : P8> : P11 {}

extension S2: P6 where A2: P7 {}

public struct S3<A2 : P8> : P11 {}

public protocol P9 {
    associatedtype A2: P7
    associatedtype A3: P11 where A3.A2 == A2
}

public protocol P3 : P9 {
    associatedtype A4: P9 where A4.A2 == A2
}

public protocol P10 {
    associatedtype A3: P11 where A3.A2: P7
}

public struct S4<T: P12> : P10 {
    public typealias A3 = T.A1.A4.A3
}

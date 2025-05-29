// RUN: %target-typecheck-verify-swift

public protocol P1 {
    associatedtype A = Void

    func makeA() -> A
    func consumeA(a: inout A)
}

extension P1 where A == Void {
    // Don't consider this witness in the 'S2: P1' conformance below.
    public func makeA() -> A { fatalError() }
}

public struct S1: P1 {}

public protocol P2: P1 where A == B.A {
    associatedtype B: P1
    var base: B { get }
}

extension P2 {
    public func makeA() -> B.A { fatalError() }
    public func consumeA(a: inout B.A) {}
}

extension S1: P2 {
    public var base: S2 { fatalError() }
}

public struct S2 {}

public struct S3 {}

extension S2: P1 {
    public typealias A = S3
}

public protocol P3: P1 where A == S3 {}

extension P3 {
    public func makeA() -> A { fatalError() }
    public func consumeA(a: inout A) {}
}

extension S2: P3 {}

let x: S3.Type = S2.A.self

// RUN: %target-typecheck-verify-swift -target %target-swift-5.1-abi-triple

public protocol P<A> {
    associatedtype A
    associatedtype B: P

    func makeA() -> A
    var b: B { get }
}

extension P where A == B.A {
    public func makeA() -> B.A {
        fatalError()
    }
}

public struct S: P {
    public var b: some P<Int> {
        return G<Int>()
    }
}

public struct G<A>: P {
    public func makeA() -> A { fatalError() }
    public var b: Never { fatalError() }
}

extension Never: P {
    public func makeA() -> Never { fatalError() }
    public var b: Never { fatalError() }
}

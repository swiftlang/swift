public protocol P {
    associatedtype A
}

public func bar<T : P>(_: T) -> T.A {
    fatalError()
}

public struct S<A> : P {}

public func foo() -> some P {
    return S<Int?>()
}


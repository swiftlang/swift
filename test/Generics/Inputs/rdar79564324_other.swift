public protocol P {
    associatedtype A: P
}

public func bar<T: P>(from: T) {}

@inlinable public func foo<T, U>(from: T, to: U) where T == T.A, U : P, U.A == T {
    bar(from: from)
}

public protocol SomeProtocol {
    associatedtype T: SomeProtocol

    func foo() -> T
}

@available(macOS 100.0, *)
@available(iOS, unavailable)
@available(tvOS, unavailable)
@available(watchOS, unavailable)
public extension SomeProtocol {
    func modify() -> some SomeProtocol {
        return NestedStruct(x: self, y: 32)
    }
}

public struct PublicStruct: SomeProtocol {
    public init<T: SomeProtocol>(_ x: T) {}

    public func foo() -> some SomeProtocol {
        return self
    }
}

struct NestedStruct<A: SomeProtocol, B>: SomeProtocol {
    init(x: A, y: B) {}

    func foo() -> some SomeProtocol {
        return self
    }
}

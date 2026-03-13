public protocol Proto {
    associatedtype T: Proto

    var value: T {get}
}

public func makeThing<P: Proto>(_ t: P) -> some Proto {
    return ProtoImpl(t.value)
}

public struct ProtoImpl<V>: Proto {
    public let genValue: V

    public init(_ genValue: V) {
        self.genValue = genValue
    }

    public var value: some Proto {
        return self
    }
}

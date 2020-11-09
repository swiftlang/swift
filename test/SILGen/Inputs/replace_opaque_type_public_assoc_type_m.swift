public protocol Gesture {
    associatedtype Body: Gesture
    var body: Body { get }

    associatedtype Value
    var value: Value { get }
}

extension Gesture {
    public var value: Body.Value { return body.value }
}

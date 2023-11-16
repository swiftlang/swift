public protocol MyProto { }

public struct MyType {
    private let value: String

    public init(_ value: String) {
        self.value = value
    }
}

extension MyType: MyProto { }

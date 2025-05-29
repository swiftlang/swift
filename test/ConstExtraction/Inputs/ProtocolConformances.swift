public protocol MyProto { }

public struct MyType {
    private let value: String

    public init(_ value: String) {
        self.value = value
    }
    
    private static let defaultValue = MyType("not me")
}

extension MyType: MyProto {
    static let anotherValue = MyType("also not me")
}

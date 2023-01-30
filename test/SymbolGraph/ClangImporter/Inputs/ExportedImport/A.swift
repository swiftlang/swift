extension SwiftStruct {
    public struct InnerStruct {}
}

extension SwiftStruct.InnerStruct {
    public struct NestedStruct {}
}

public protocol SomeProtocol {}

extension SomeProtocol {
    public func someFunc() {}
}

extension SwiftStruct.InnerStruct.NestedStruct: SomeProtocol {}

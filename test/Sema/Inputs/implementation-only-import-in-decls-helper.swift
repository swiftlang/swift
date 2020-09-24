import NormalLibrary

extension NormalStruct: NormalProto {
  public typealias Assoc = Int
}
extension GenericStruct: NormalProto {
  public typealias Assoc = Int
}
extension NormalClass: NormalProto {
  public typealias Assoc = Int
}

public struct BadStruct {}
public protocol BadProto {}
open class BadClass {}

public struct IntLike: ExpressibleByIntegerLiteral, Equatable {
  public init(integerLiteral: Int) {}
}

@propertyWrapper
public struct BadWrapper {
    public var wrappedValue: Int
    public init(wrappedValue: Int) {
        self.wrappedValue = wrappedValue
    }
}

precedencegroup BadPrecedence {}

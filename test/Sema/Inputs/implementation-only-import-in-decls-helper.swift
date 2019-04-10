public struct BadStruct {}
public protocol BadProto {}
open class BadClass {}

public struct IntLike: ExpressibleByIntegerLiteral, Equatable {
  public init(integerLiteral: Int) {}
}

precedencegroup BadPrecedence {}

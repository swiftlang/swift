open class SomeClass {
  open class NestedClass { public init() {} }

  public static func staticFunc1() -> Int {}
  public static var staticVar1: Int
  public init() {}
}
public struct SomeStruct {
  public init() {}
  public init(a: Int) {}
}
public enum SomeEnum {
  case Foo
}
public protocol SomeProtocol {
  associatedtype Foo
}
public protocol SomeExistential {
}
public class SomeProtocolImpl : SomeProtocol {}
public typealias SomeTypealias = Swift.Int
public var someGlobal: Int
public func someFunc() {}


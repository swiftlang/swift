public protocol Fooable {
  func foo<T>(_ x: T)
}

public class FooClass : Fooable {
  public init() { }
  public func foo<U>(_ x: U) {}
}

public struct FooStruct : Fooable {
  public func foo<V>(_ x: V) {}
  public init() {}
}


public protocol Barrable {
  func bar<T>(_ x: Self, y: T)
}

public class BarClass : Barrable {
  public init() { }
  public func bar<U>(_ x: BarClass, y: U) { }
}

public struct BarStruct : Barrable {
  public var x = 0
  public func bar<V>(_ x: BarStruct, y: V) { }
  public init() {}
}


public protocol HasAssociatedType {
  associatedtype Foo : Fooable
}

public protocol Bassable {
  func bas<T : HasAssociatedType>(_ x: T, y: T.Foo)
}

public class BasClass : Bassable {
  public init() { }
  public func bas<U : HasAssociatedType>(_ x: U, y: U.Foo) {}
}

public struct BasStruct : Bassable {
  public func bas<V : HasAssociatedType>(_ x: V, y: V.Foo) {}
  public init() {}
}


prefix operator ~~~

public protocol _CyclicAssociated {
  associatedtype Assoc = CyclicImpl
}

public protocol CyclicAssociated : _CyclicAssociated {
  static prefix func ~~~(_: Self.Type)
}

prefix public func ~~~ <T: _CyclicAssociated>(_: T.Type) {}

public struct CyclicImpl : CyclicAssociated {
  public typealias Assoc = CyclicImpl
  public init() {}
}

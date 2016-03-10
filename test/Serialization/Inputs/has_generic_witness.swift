public protocol Fooable {
  func foo<T>(x: T)
}

public class FooClass : Fooable {
  public init() { }
  public func foo<U>(x: U) {}
}

public struct FooStruct : Fooable {
  public func foo<V>(x: V) {}
  public init() {}
}


public protocol Barrable {
  func bar<T>(x: Self, y: T)
}

public class BarClass : Barrable {
  public init() { }
  public func bar<U>(x: BarClass, y: U) { }
}

public struct BarStruct : Barrable {
  public var x = 0
  public func bar<V>(x: BarStruct, y: V) { }
  public init() {}
}


public protocol HasAssociatedType {
  associatedtype Foo : Fooable
}

public protocol Bassable {
  func bas<T : HasAssociatedType>(x: T, y: T.Foo)
}

public class BasClass : Bassable {
  public init() { }
  public func bas<U : HasAssociatedType>(x: U, y: U.Foo) {}
}

public struct BasStruct : Bassable {
  public func bas<V : HasAssociatedType>(x: V, y: V.Foo) {}
  public init() {}
}


prefix operator ~~~ {}

public protocol _CyclicAssociated {
  associatedtype Assoc = CyclicImpl
}

public protocol CyclicAssociated : _CyclicAssociated {
  prefix func ~~~(_: Self.Type)
}

prefix public func ~~~ <T: _CyclicAssociated>(_: T.Type) {}

public struct CyclicImpl : CyclicAssociated {
  public typealias Assoc = CyclicImpl
  public init() {}
}

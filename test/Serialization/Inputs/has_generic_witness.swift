protocol Fooable {
  func foo<T>(x: T)
}

class FooClass : Fooable {
  init() { }
  func foo<U>(x: U) {}
}

struct FooStruct : Fooable {
  func foo<V>(x: V) {}
}


protocol Barrable {
  func bar<T>(x: Self, y: T)
}

class BarClass : Barrable {
  init() { }
  func bar<U>(x: BarClass, y: U) { }
}

struct BarStruct : Barrable {
  var x = 0
  func bar<V>(x: BarStruct, y: V) { }
}


protocol HasAssociatedType {
  typealias Foo : Fooable
}

protocol Bassable {
  func bas<T : HasAssociatedType>(x: T, y: T.Foo)
}

class BasClass : Bassable {
  init() { }
  func bas<U : HasAssociatedType>(x: U, y: U.Foo) {}
}

struct BasStruct : Bassable {
  func bas<V : HasAssociatedType>(x: V, y: V.Foo) {}
}


operator prefix ~~~ {}

protocol _CyclicAssociatedType {
  typealias Assoc = CyclicImpl
}

protocol CyclicAssociatedType : _CyclicAssociatedType {
  @prefix func ~~~(_: Self.Type)
}

@prefix func ~~~ <T: _CyclicAssociatedType>(_: T.Type) {}

struct CyclicImpl : CyclicAssociatedType {
  typealias Assoc = CyclicImpl
}

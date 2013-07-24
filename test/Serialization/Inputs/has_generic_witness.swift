protocol Fooable {
  func foo<T>(x:T)
}

class FooClass : Fooable {
  func foo<U>(x:U) {}
}

struct FooStruct : Fooable {
  func foo<V>(x:V) {}
}


protocol Barrable {
  func bar<T>(x:This, y:T)
}

class BarClass : Barrable {
  func bar<U>(x:BarClass, y:U) { }
}

struct BarStruct : Barrable {
  var x:Int
  func bar<V>(x:BarStruct, y:V) { }
}


protocol HasAssociatedType {
  typealias Foo : Fooable
}

protocol Bassable {
  func bas<T:HasAssociatedType>(x:T, y:T.Foo)
}

class BasClass : Bassable {
  func bas<U:HasAssociatedType>(x:U, y:U.Foo) {}
}

struct BasStruct : Bassable {
  func bas<V:HasAssociatedType>(x:V, y:V.Foo) {}
}

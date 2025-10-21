// RUN: %target-swift-emit-silgen %s

class C {
  func f1() -> Self? {
    return D<Self>().g(self)
  }

  func f2() -> Self? {
    return D<Self>()[self]
  }

  func f3() -> Self? {
    return D<Self>().x
  }
}

class D<T> {
  func g(_ t: T) -> T? {
    return t
  }

  subscript(_ t: T) -> T? {
    return t
  }

  var x: T? {
    fatalError()
  }
}

// RUN: %target-swift-frontend -disable-availability-checking -emit-ir -verify %s

// rdar://problem/53318811

class Foo<T> {
  var x: T { fatalError() }
}

func foo<T>(_: T) -> some Foo<T> {
  let localProp: some Foo<T> = Foo()
  return localProp
}

class C<T> {
  func bar() -> some Foo<T> {
    return Foo()
  }

  var prop: some Foo<T> = Foo()
}

func bar() -> Int {
  var x = 0
  x = foo(0).x
  x = C<Int>().bar().x
  x = C<Int>().prop.x
  return x
}

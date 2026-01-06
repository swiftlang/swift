// RUN: %target-run-simple-swift(-enable-experimental-feature Embedded -parse-as-library -runtime-compatibility-version none -wmo -Xfrontend -disable-objc-interop) | %FileCheck %s
// RUN: %target-run-simple-swift(-Osize -enable-experimental-feature Embedded -parse-as-library -runtime-compatibility-version none -wmo -Xfrontend -disable-objc-interop) | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: swift_feature_Embedded

protocol Fooable {
  func foo()
}

class GenericFooableClass<T>: Fooable {
  func foo() { print("GenericFooableClass<T>.foo") }
}

class GenericFooableSubClass<T>: GenericFooableClass<T> {
  override func foo() { print("GenericFooableSubClass<T>.foo") }
}

func makeItFoo<F: Fooable>(f: F) {
  f.foo()
}

class BaseClass<A> {
  func test() {}
}

class SubClass1<B>: BaseClass<Int> {
  override func test() {}
}

class SubClass2 : SubClass1<Int> {
  override func test() { print("SubClass2") }
}

public class Outer<T> {
  public class Inner {
    func foo() {
      print("Inner.foo")
    }
  }
}

public func makeInner() -> Outer<Int>.Inner {
  return Outer<Int>.Inner()
}

final class List<Element> where Element: ~Copyable {
  init(x: Element) where Element: Copyable { }
}

func testList() -> List<Int> {
  return List(x: 0)
}

open class OpenClass<Element> where Element: ~Copyable {
  public func foo(x: Element) where Element: Copyable { }
}

func testOpenClass() -> OpenClass<Int> {
  return OpenClass()
}


class Base<T> {
  func foo(_: T) {}
}

class Derived<T>: Base<Array<T>> {}

func testBaseDerived() -> Derived<Int> {
  return Derived()
}

class Base2<T> {
  func foo(_: T) {}
}

class Derived2<T>: Base2<(T, T)> {}

func testBaseDerived2() -> Derived2<Int> {
  return Derived2()
}

class Base3<T> {
  func foo(_: T) {}
}
class Derived3<T, U>: Base3<(T, U)> {}

func testBaseDerived3() -> Derived3<Int, Bool> {
  return Derived3()
}

@main
struct Main {
  static func main() {
    let f = GenericFooableClass<Int>()
    makeItFoo(f: f)
    let g: GenericFooableClass = GenericFooableSubClass<Int>()
    makeItFoo(f: g)
    let x = SubClass2()
    x.test()
    makeInner().foo()
    testList()
    testOpenClass()
    testBaseDerived()
    testBaseDerived2()
    testBaseDerived3()
  }
}

// CHECK: GenericFooableClass<T>.foo
// CHECK: GenericFooableSubClass<T>.foo
// CHECK: SubClass2
// CHECK: Inner.foo


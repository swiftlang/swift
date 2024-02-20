// RUN: %target-run-simple-swift(-enable-experimental-feature Embedded -parse-as-library -runtime-compatibility-version none -wmo -Xfrontend -disable-objc-interop) | %FileCheck %s
// RUN: %target-run-simple-swift(-Osize -enable-experimental-feature Embedded -parse-as-library -runtime-compatibility-version none -wmo -Xfrontend -disable-objc-interop) | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx || OS=linux-gnu

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

@main
struct Main {
  static func main() {
    let f = GenericFooableClass<Int>()
    makeItFoo(f: f)
    let g: GenericFooableClass = GenericFooableSubClass<Int>()
    makeItFoo(f: g)
    let x = SubClass2()
    x.test()
  }
}

// CHECK: GenericFooableClass<T>.foo
// CHECK: GenericFooableSubClass<T>.foo
// CHECK: SubClass2


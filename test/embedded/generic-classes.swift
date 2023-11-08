// RUN: %target-run-simple-swift(%S/Inputs/print.swift -enable-experimental-feature Embedded -parse-as-library -runtime-compatibility-version none -wmo -Xfrontend -disable-objc-interop) | %FileCheck %s
// RUN: %target-run-simple-swift(-Osize %S/Inputs/print.swift -enable-experimental-feature Embedded -parse-as-library -runtime-compatibility-version none -wmo -Xfrontend -disable-objc-interop) | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: VENDOR=apple
// REQUIRES: OS=macosx

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

@main
struct Main {
  static func main() {
    let f = GenericFooableClass<Int>()
    makeItFoo(f: f)
    let g: GenericFooableClass = GenericFooableSubClass<Int>()
    makeItFoo(f: g)
  }
}

// CHECK: GenericFooableClass<T>.foo
// CHECK: GenericFooableSubClass<T>.foo


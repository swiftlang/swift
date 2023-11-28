// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s %S/Inputs/print.swift -enable-experimental-feature Embedded -c -o %t/main.o
// RUN: %target-clang %t/main.o -o %t/a.out -dead_strip
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: VENDOR=apple
// REQUIRES: OS=macosx

class MyClass {
  init() { print("MyClass.init") }
  deinit { print("MyClass.deinit") }
  func foo() { print("MyClass.foo") }
}

class MySubClass: MyClass {
  override init() { print("MySubClass.init") }
  deinit { print("MySubClass.deinit") }
  override func foo() { print("MySubClass.foo") }
}

class MySubSubClass: MySubClass {
  override init() { print("MySubSubClass.init") }
  deinit { print("MySubSubClass.deinit") }
  override func foo() { print("MySubSubClass.foo") }
}

@main
struct Main {
  static var o: (MyClass?, MyClass?, MyClass?) = (nil, nil, nil)

  static func main() {
    print("1") // CHECK: 1
    o.0 = MyClass()
    // CHECK: MyClass.init
    print("")

    print("2") // CHECK: 2
    o.1 = MySubClass()
    // CHECK: MySubClass.init
    // CHECK: MyClass.init
    print("")

    print("3") // CHECK: 3
    o.2 = MySubSubClass()
    // CHECK: MySubSubClass.init
    // CHECK: MySubClass.init
    // CHECK: MyClass.init
    print("")

    print("4") // CHECK: 4
    o.0!.foo()
    o.1!.foo()
    o.2!.foo()
    // CHECK: MyClass.foo
    // CHECK: MySubClass.foo
    // CHECK: MySubSubClass.foo
    print("")
    
    print("5") // CHECK: 5
    o.0 = nil
    // CHECK: MyClass.deinit
    o.1 = nil
    // CHECK: MySubClass.deinit
    // CHECK: MyClass.deinit
    o.2 = nil
    // CHECK: MySubSubClass.deinit
    // CHECK: MySubClass.deinit
    // CHECK: MyClass.deinit
    print("")
  }
}

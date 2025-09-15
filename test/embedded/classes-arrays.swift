// RUN: %empty-directory(%t)
// RUN: %target-run-simple-swift(-parse-as-library -enable-experimental-feature Embedded -wmo) | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: swift_feature_Embedded

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
  static var objects: [MyClass] = []
  static func main() {
    print("1") // CHECK: 1
    objects.append(MyClass())
    // CHECK: MyClass.init
    print("")

    print("2") // CHECK: 2
    objects.append(MySubClass())
    // CHECK: MySubClass.init
    // CHECK: MyClass.init
    print("")

    print("3") // CHECK: 3
    objects.append(MySubSubClass())
    // CHECK: MySubSubClass.init
    // CHECK: MySubClass.init
    // CHECK: MyClass.init
    print("")

    print("4") // CHECK: 4
    for o in objects {
      o.foo()
      // CHECK: MyClass.foo
      // CHECK: MySubClass.foo
      // CHECK: MySubSubClass.foo
    }
    print("")

    print("5") // CHECK: 5
    objects = []
    // CHECK: MyClass.deinit
    // CHECK: MySubClass.deinit
    // CHECK: MyClass.deinit
    // CHECK: MySubSubClass.deinit
    // CHECK: MySubClass.deinit
    // CHECK: MyClass.deinit
    print("")
  }
}

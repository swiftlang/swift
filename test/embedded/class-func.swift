// RUN: %target-run-simple-swift(-enable-experimental-feature Embedded -parse-as-library -runtime-compatibility-version none -wmo -Xfrontend -disable-objc-interop) | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: swift_feature_Embedded

@main
struct Main {
     static func main() {
          let array: [MyClass] = [MyClass(), MySubclass()]
          for e in array {
               e.method()
          }
     }
}

class MyClass {
    func method() { Self.foo() }

    class func foo() { print("MyClass.foo") }
}

class MySubclass: MyClass {

    override class func foo() { print("MySubclass.foo") }
}

// CHECK: MyClass.foo
// CHECK: MySubclass.foo

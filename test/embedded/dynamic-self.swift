// RUN: %target-swift-emit-ir %s -enable-experimental-feature Embedded -parse-as-library -module-name main | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: VENDOR=apple
// REQUIRES: OS=macosx

class MyClass {
     init() {
          Self.static_foo()
     }

     static func static_foo() {}
}

@main
struct Main {
     static func main() {
          _ = MyClass()
     }
}

// CHECK: define {{.*}}@main(

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -parse-as-library -enable-experimental-feature Embedded -c -o %t/main.o
// RUN: %target-clang %target-clang-resource-dir-opt %t/main.o -o %t/a.out -dead_strip
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: swift_feature_Embedded

public class MyClass {
  var handler: (()->())? = nil
  func foo() {
    handler?()
  }
  deinit { print("deinit") }
}

@main
struct Main {
  static var o: MyClass? = nil

  static func main() {
    o = MyClass()
    o!.handler = { print("no captures") }
    o!.foo() // CHECK: no captures
    o = nil // CHECK: deinit

    var local = 42
    o = MyClass()
    o!.handler = { print("capture local"); local += 1 }
    o!.foo() // CHECK: capture local
    print(local == 43 ? "43" : "???") // CHECK: 43
    o = nil // CHECK: deinit
  }
}

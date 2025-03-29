// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple %s -emit-irgen -g -o - | %FileCheck %s
// REQUIRES: concurrency

func some_func(_: () -> Void) async {}

class Model {
  static func SelfFunction() {}

  func foo() async -> () {
    let somevar = 10

    return await some_func {
      Self.SelfFunction()
    }
  }
}

// Check that the load of DynamicSelf in foo does not have any debug information, as
// this is hoisted to the start of the function.
// CHECK: define {{.*}} @"$s19DynamicSelfLocation5ModelC3fooyyYaF"({{.*}}, ptr swiftself %[[Self:.*]])
// CHECK-NEXT: entry:
// CHECK-NEXT:   %2 = load ptr, ptr %[[Self]]
// CHECK-NOT:    !dbg
// CHECK-SAME:  {{$}}

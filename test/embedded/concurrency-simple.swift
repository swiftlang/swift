// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -parse-as-library %s -c -o %t/a.o
// RUN: %target-clang %t/a.o -o %t/a.out -L%swift_obj_root/lib/swift/embedded/%target-cpu-apple-macos -lswift_Concurrency -lswift_ConcurrencyDefaultExecutor -dead_strip
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_in_compiler
// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx
// REQUIRES: swift_feature_Embedded

import _Concurrency

public func test() async -> Int {
  print("test")
  let t = Task {
    print("return 42")
    return 42
  }
  print("await")
  let v = await t.value
  print("return")
  return v
}

@main
struct Main {
  static func main() async {
    print("main")
    // CHECK: main
    let t = Task {
      print("task")
      let x = await test()
      print(x == 42 ? "42" : "???")
    }
    print("after task")
    await t.value
    // CHECK-NEXT: after task
    // CHECK-NEXT: task
    // CHECK-NEXT: test
    // CHECK-NEXT: await
    // CHECK-NEXT: return 42
    // CHECK-NEXT: return
    // CHECK-NEXT: 42
  }
}

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -parse-as-library %s -c -o %t/a.o
// RUN: %target-clang -x c -std=c11 -I %swift_obj_root/include -c %S/Inputs/executor.c -o %t/executor.o
// RUN: %target-clang %t/a.o %t/executor.o -o %t/a.out %swift_obj_root/lib/swift/embedded/%target-cpu-apple-macos/libswift_Concurrency.a -dead_strip
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_in_compiler
// REQUIRES: optimized_stdlib
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
    // CHECK-NEXT: executor: job [[JOB1:0x[0-9a-f]+]] enqueued
    print("after task")
    // CHECK-NEXT: after task
    await t.value
    // CHECK-NEXT: executor: running
    // CHECK-NEXT: executor: job [[JOB1]] running
    // CHECK-NEXT: task
    // CHECK-NEXT: test
    // CHECK-NEXT: executor: job [[JOB2:0x[0-9a-f]+]] enqueued
    // CHECK-NEXT: await
    // CHECK-NEXT: executor: job [[JOB2]] running
    // CHECK-NEXT: return 42
    // CHECK-NEXT: executor: job [[JOB1]] enqueued
    // CHECK-NEXT: executor: job [[JOB1]] running
    // CHECK-NEXT: return
    // CHECK-NEXT: 42
    // CHECK-NEXT: executor: job [[JOB3:0x[0-9a-f]+]] enqueued
    // CHECK-NEXT: executor: job [[JOB3]] running
  }
}

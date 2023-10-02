// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -target %target-cpu-apple-macos14 -enable-experimental-feature Embedded -parse-as-library %s %S/Inputs/print.swift -c -o %t/a.o
// RUN: %target-clang -x c -c %S/Inputs/print.c -o %t/print.o
// RUN: %target-clang %t/a.o %t/print.o -o %t/a.out %swift_obj_root/lib/swift/embedded/%target-cpu-apple-macos/libswift_Concurrency.a -dead_strip
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: optimized_stdlib
// REQUIRES: VENDOR=apple
// REQUIRES: OS=macosx

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

@_silgen_name("swift_task_asyncMainDrainQueue")
func _asyncMainDrainQueue() -> Never

@main
struct Main {
  static func main() {
    print("main")
    // CHECK: main

    Task {
      print("task")
      let x = await test()
      print(x == 42 ? "42" : "???")
    }
    print("drain")
    // CHECK-NEXT: drain

    _asyncMainDrainQueue()
    // CHECK-NEXT: task
    // CHECK-NEXT: test
    // CHECK-NEXT: await
    // CHECK-NEXT: return 42
    // CHECK-NEXT: return
    // CHECK-NEXT: 42

    print("done")
    // CHECK-NOT: done
  }
}

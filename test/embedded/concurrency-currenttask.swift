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

func printCurrentTaskNilOrNonNil() {
  withUnsafeCurrentTask { task in
    print(task == nil ? "nil" : "valid")
  }
}

public func test() async -> Int {
  printCurrentTaskNilOrNonNil()
  return 42
}

@main
struct Main {
  static func main() async {
    print("main")
    // CHECK: main

    printCurrentTaskNilOrNonNil()
    let t = Task {
      let x = await test()
      print(x == 42 ? "42" : "???")
    }
    await t.value
    // CHECK-NEXT: valid
    // CHECK-NEXT: valid
    // CHECK-NEXT: 42
  }
}

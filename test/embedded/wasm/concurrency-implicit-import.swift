// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -parse-as-library %s -c -o %t/a.o
// RUN: %target-clang %target-clang-resource-dir-opt %t/a.o %target-embedded-posix-shim -o %t/a.out -L%swift_obj_root/lib/swift/embedded/%module-target-triple -lc++ -lswift_Concurrency %target-swift-default-executor-opt -dead_strip
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_in_compiler
// REQUIRES: optimized_stdlib
// REQUIRES: OS=wasip1
// REQUIRES: swift_feature_Embedded

// Verify that _Concurrency is implicitly imported for Embedded Swift on wasip1.

public func test() async -> Int {
  let t = Task {
    return 42
  }
  return await t.value
}

@main
struct Main {
  static func main() async {
    let x = await test()
    print(x == 42 ? "OK" : "FAIL")
    // CHECK: OK
  }
}

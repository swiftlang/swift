// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -parse-as-library %s -c -o %t/a.o
// RUN: %target-clang %t/a.o -o %t/a.out -L%swift_obj_root/lib/swift/embedded/%module-target-triple %target-clang-resource-dir-opt -lswift_Concurrency %target-swift-default-executor-opt -dead_strip
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_in_compiler
// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx || OS=wasip1
// REQUIRES: swift_feature_Embedded

import _Concurrency

public func test1() async -> Int {
  let x = await withUnsafeContinuation { continuation in
    continuation.resume(returning: 42)
  }
  return x
}

public func test2() async -> Int {
  let x = await withCheckedContinuation { continuation in
    continuation.resume(returning: 777)
  }
  return x
}

@main
struct Main {
  static func main() async {
    let x = await test1()
    print(x)
    let y = await test2()
    print(y)

    // CHECK: 42
    // CHECK: 777
  }
}

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -parse-as-library %s -c -o %t/a.o
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/a.o -o %t/a.out -L%swift_obj_root/lib/swift/embedded/%module-target-triple -lc++ -lswift_Concurrency %target-swift-default-executor-opt %target-embedded-concurrency-threading-shim -dead_strip
// RUN: %target-run %t/a.out | %FileCheck %s
// RUN: %if threading_pthreads %{ %llvm-nm --defined-only %t/a.out | %FileCheck %s --check-prefix=PTHREADS %}
// RUN: %if !threading_pthreads %{ %llvm-nm --defined-only %t/a.out | %FileCheck %s --check-prefix=SINGLE %}

// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: OS=wasip1
// REQUIRES: swift_feature_Embedded

// Of the archives on this link line, only EmbeddedPlatformMultiThreadedPOSIX
// calls pthread_mutexattr_settype.
// PTHREADS: pthread_mutexattr_settype
// SINGLE-NOT: pthread_mutexattr_settype

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

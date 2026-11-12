// RUN: %empty-directory(%t)
// RUN: %target-clang -x c -c %S/Inputs/unbuffered-putchar.c -o %t/unbuffered-putchar.o
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -parse-as-library %s -c -o %t/a.o
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/a.o %t/unbuffered-putchar.o -o %t/a.out -L%swift_obj_root/lib/swift/embedded/%module-target-triple -lc++ -lswift_Concurrency %target-swift-default-executor-opt %target-embedded-concurrency-threading-shim -dead_strip
// RUN: %target-not-crash %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx || OS=wasip1
// REQUIRES: swift_feature_Embedded

@_silgen_name("swift_task_donateThreadToGlobalExecutorUntil")
func donateThreadToGlobalExecutorUntil(
  _ condition: @convention(c) (UnsafeMutableRawPointer?) -> Bool,
  _ context: UnsafeMutableRawPointer?)

@main
struct Main {
  static func main() {
    print("before")
    donateThreadToGlobalExecutorUntil({ _ in true }, nil)
    print("after")
  }
}

// CHECK: before
// CHECK-NOT: after

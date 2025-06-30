// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -parse-as-library %s -c -o %t/a.o -g -O
// RUN: %target-clang -x c -std=c11 -c %S/Inputs/debug-malloc.c -o %t/debug-malloc.o -g
// RUN: %target-clang %t/a.o %t/debug-malloc.o -o %t/a.out -L%swift_obj_root/lib/swift/embedded/%target-cpu-apple-macos -lswift_Concurrency -lswift_ConcurrencyDefaultExecutor -dead_strip -g
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_in_compiler
// REQUIRES: optimized_stdlib
// REQUIRES: swift_feature_Embedded

import _Concurrency

@main
struct Main {
  static func main() async {
    print("start")
    // CHECK: start
    do {
        let x = Task {
            return 42
        }
        _ = await x.value
    }
    // There should be exactly 4 allocations involved:
    // - 2x 32 bytes ... closure context for swift_task_create, closure object to pass to Task.init
    // - 1x 320 bytes ... malloc(amountToAllocate) in swift_task_create_common for the Task heap object itself
    // - 1x 1016 bytes ... the initial StackAllocator slab in the task-specific allocator
    // Check that they are all accounted for and free'd.

    // CHECK: malloc({{[0-9]+}})-> [[M1:0x[0-9a-f]+]]
    // CHECK: malloc({{[0-9]+}})-> [[M2:0x[0-9a-f]+]]
    // CHECK: malloc({{[0-9]+}})-> [[M3:0x[0-9a-f]+]]
    // CHECK: malloc({{[0-9]+}})-> [[M4:0x[0-9a-f]+]]
    // CHECK: free([[M1]])
    // CHECK: free([[M2]])
    // CHECK: free([[M4]])
    // CHECK: free([[M3]])

    print("done")
    // CHECK: done
  }
}

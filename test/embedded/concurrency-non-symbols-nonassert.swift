// RUN: %llvm-nm -a %swift_obj_root/lib/swift/embedded/%module-target-triple/libswift_Concurrency.a | %FileCheck %s --dump-input fail

// REQUIRES: swift_in_compiler
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_stdlib_no_asserts
// REQUIRES: no_asserts
// REQUIRES: optimized_stdlib

// Check for symbols that we explicitly don't want in the Embedded Swift
// concurrency library.

// CHECK-NOT: abort

// CHECK-DAG: swift_fatalError
// CHECK-DAG: swift_slowAlloc
// CHECK-DAG: swift_slowDealloc

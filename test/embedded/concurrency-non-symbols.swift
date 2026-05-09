// RUN: %llvm-nm -a %swift_obj_root/lib/swift/embedded/%module-target-triple/libswift_Concurrency.a | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: swift_feature_Embedded

// Check for symbols that we explicitly don't want in the Embedded Swift
// concurrency library.

// CHECK-NOT: swift_reportToDebugger
// CHECK-NOT: swift_shouldReportFatalErrorsToDebugger
// CHECK-NOT: swift_reportError
// CHECK-NOT: getResilientMetadataBounds
// CHECK-NOT: puts
// CHECK-NOT: strlen
// CHECK-NOT: strncpy
// CHECK-NOT: vprintf
// CHECK-NOT: vsnprintf
// CHECK-NOT: malloc
// CHECK-NOT: free
// CHECK-NOT: __cxa_pure_virtual

// CHECK-DAG: swift_fatalError
// CHECK-DAG: swift_slowAlloc
// CHECK-DAG: swift_slowDealloc

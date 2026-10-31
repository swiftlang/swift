// RUN: %llvm-nm -u %swift_obj_root/lib/swift/embedded/%module-target-triple/libswift_Concurrency.a | %FileCheck %s
// RUN: %llvm-nm -u %swift_obj_root/lib/swift/embedded/%module-target-triple/libswift_Concurrency.a | %FileCheck %s --check-prefix=EXCLUDES

// REQUIRES: swift_feature_Embedded
// REQUIRES: OS=macosx || OS=wasip1

// CHECK-DAG: _swift_clockContinuous_getTime
// CHECK-DAG: _swift_clockContinuous_getResolution
// CHECK-DAG: _swift_clockSuspending_getTime
// CHECK-DAG: _swift_clockSuspending_getResolution

// EXCLUDES-NOT: clock_gettime
// EXCLUDES-NOT: clock_getres
// EXCLUDES-NOT: nanosleep
// EXCLUDES-NOT: __error

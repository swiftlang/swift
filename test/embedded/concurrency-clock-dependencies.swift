// RUN: %llvm-nm -u %swift_obj_root/lib/swift/embedded/%module-target-triple/libswift_Concurrency.a | %FileCheck %s --implicit-check-not=clock_gettime --implicit-check-not=clock_getres --implicit-check-not=nanosleep --implicit-check-not=__error

// REQUIRES: swift_feature_Embedded
// REQUIRES: OS=macosx || OS=wasip1

// CHECK-DAG: _swift_clockContinuous_getTime
// CHECK-DAG: _swift_clockContinuous_getResolution
// CHECK-DAG: _swift_clockSuspending_getTime
// CHECK-DAG: _swift_clockSuspending_getResolution
// CHECK-DAG: _swift_clock_sleep

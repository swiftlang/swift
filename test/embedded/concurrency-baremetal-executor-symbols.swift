// RUN: %llvm-nm %swift_obj_root/lib/swift/embedded/armv7em-none-none-eabi/libswift_Concurrency.a | %FileCheck %s

// REQUIRES: OS=macosx
// REQUIRES: CODEGENERATOR=ARM
// REQUIRES: embedded_stdlib_cross_compiling

// CHECK-LABEL: Actor.cpp.o:
// CHECK: U _swift_task_enqueueOnExecutor

// CHECK-LABEL: ConcurrencyHooks.cpp.o:
// CHECK-DAG: W _swift_concurrency_mainExecutorIsGlobal
// CHECK-DAG: U swift_task_getMainExecutorImpl

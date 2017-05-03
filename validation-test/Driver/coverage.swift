// RUN: rm -rf %t && mkdir %t

// RUN: %target-build-swift %s -profile-generate -o %t/main
// RUN: (cd %t && %target-run %t/main)

// RUN: llvm-profdata show -function=thisFunctionIsRun %t/*.profraw | FileCheck %s -check-prefix=CHECK-RUN-FN
// RUN: llvm-profdata show -function=thisFunctionIsNotRun %t/*.profraw | FileCheck %s -check-prefix=CHECK-NOT-RUN-FN

// REQUIRES: compiler_rt

func thisFunctionIsRun() {
  // CHECK-RUN-FN: Counters: 1
  // CHECK-RUN-FN: Function count: 1
  print("hello")
}

func thisFunctionIsNotRun() {
  // CHECK-NOT-RUN-FN: Counters: 1
  // CHECK-NOT-RUN-FN: Function count: 0
  print("goodbye")
}

thisFunctionIsRun()

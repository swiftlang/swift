// RUN: %empty-directory(%t)
// RUN: %target-build-swift -emit-module  -Xfrontend -disable-availability-checking -Xfrontend -disable-diagnostic-passes -whole-module-optimization -Xfrontend -enable-objc-interop -o %t/def_concurrency.swiftmodule %S/Inputs/def_concurrency.sil
// RUN: llvm-bcanalyzer %t/def_concurrency.swiftmodule | %FileCheck %s
// RUN: %target-build-swift -emit-sil -I %t %s -o %t/concurrency_sil.sil
// RUN: %target-sil-opt -I %t %t/concurrency_sil.sil -performance-linker | %FileCheck %S/Inputs/def_concurrency.sil

// This test currently is written such that no optimizations are assumed.
// REQUIRES: swift_test_mode_optimize_none
// REQUIRES: concurrency

// CHECK-NOT: UnknownCode

import def_concurrency

func test_all() {
  serialize_all()
}

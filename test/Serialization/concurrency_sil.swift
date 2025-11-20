// RUN: %empty-directory(%t)
// RUN: %target-build-swift -Xllvm -sil-disable-pass=simplification -emit-module  -target %target-swift-5.1-abi-triple -Xfrontend -disable-diagnostic-passes -whole-module-optimization -Xfrontend -enable-objc-interop -o %t/def_concurrency.swiftmodule %S/Inputs/def_concurrency.sil
// RUN: llvm-bcanalyzer %t/def_concurrency.swiftmodule | %FileCheck %s
// RUN: %target-build-swift -Xllvm -sil-print-types -emit-sil -I %t %s -o %t/concurrency_sil.sil
// RUN: %target-sil-opt -sil-print-types -parse-serialized-sil -I %t %t/concurrency_sil.sil -performance-linker | %FileCheck %S/Inputs/def_concurrency.sil

// This test currently is written such that no optimizations are assumed.
// REQUIRES: swift_test_mode_optimize_none
// REQUIRES: concurrency

// CHECK-NOT: UnknownCode

import def_concurrency

func test_all() {
  serialize_all()
}

// RUN: %empty-directory(%t)
// RUN: %target-build-swift -Xllvm -sil-disable-pass=simplification -emit-module -Xfrontend -disable-diagnostic-passes -whole-module-optimization -Xfrontend -enable-objc-interop -o %t/def_basic.swiftmodule %S/Inputs/def_basic.sil
// RUN: llvm-bcanalyzer %t/def_basic.swiftmodule | %FileCheck %s
// RUN: %target-build-swift -Xllvm -sil-print-types -emit-sil -I %t %s -o %t/basic_sil.sil
// RUN: %target-sil-opt -sil-print-types -parse-serialized-sil -I %t %t/basic_sil.sil -performance-linker | %FileCheck %S/Inputs/def_basic.sil

// This test currently is written such that no optimizations are assumed.
// REQUIRES: swift_test_mode_optimize_none

// CHECK-NOT: UnknownCode

// Inputs/def_basic.sil is based on basic.sil under test/SIL/Parser.

import def_basic

func test_all() {
  serialize_all()
}

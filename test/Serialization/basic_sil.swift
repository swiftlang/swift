// RUN: %empty-directory(%t)
// RUN: %target-build-swift -Xfrontend -assume-parsing-unqualified-ownership-sil -emit-module -Xfrontend -disable-diagnostic-passes -force-single-frontend-invocation -Xfrontend -enable-objc-interop -o %t/def_basic.swiftmodule %S/Inputs/def_basic.sil
// RUN: llvm-bcanalyzer %t/def_basic.swiftmodule | %FileCheck %s
// RUN: %target-build-swift -emit-sil -I %t %s -o %t/basic_sil.sil
// RUN: %target-sil-opt -assume-parsing-unqualified-ownership-sil -I %t %t/basic_sil.sil -performance-linker | %FileCheck %S/Inputs/def_basic.sil

// This test currently is written such that no optimizations are assumed.
// REQUIRES: swift_test_mode_optimize_none

// CHECK-NOT: UnknownCode

// Inputs/def_basic.sil is based on basic.sil under test/SIL/Parser.

import def_basic

func test_all() {
  serialize_all()
}

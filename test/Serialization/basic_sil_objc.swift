// RUN: %empty-directory(%t)
// RUN: %target-build-swift -Xllvm -sil-disable-pass=simplification -Xfrontend %clang-importer-sdk -I %S/../Inputs/clang-importer-sdk/swift-modules -emit-module -Xfrontend -disable-diagnostic-passes -whole-module-optimization -o %t/def_basic_objc.swiftmodule %S/Inputs/def_basic_objc.sil
// RUN: llvm-bcanalyzer %t/def_basic_objc.swiftmodule | %FileCheck %s

// RUN: %target-build-swift -Xfrontend %clang-importer-sdk -Xllvm -sil-print-types -emit-sil -I %t %s -o %t/basic_sil_objc.sil
// RUN: %target-sil-opt -sil-print-types -parse-serialized-sil %t/basic_sil_objc.sil -performance-linker -I %t | %FileCheck %S/Inputs/def_basic_objc.sil

// This test currently is written such that no optimizations are assumed.
// REQUIRES: swift_test_mode_optimize_none
// REQUIRES: objc_interop

// CHECK-NOT: UnknownCode

import def_basic_objc
import Foundation

func test_all() {
  serialize_all()
}

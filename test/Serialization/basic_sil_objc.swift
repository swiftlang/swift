// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: %target-build-swift -Xllvm -new-mangling-for-tests -Xfrontend %clang-importer-sdk -I %S/../Inputs/clang-importer-sdk/swift-modules -emit-module -Xfrontend -disable-diagnostic-passes -Xfrontend -sil-serialize-all -force-single-frontend-invocation -o %t/def_basic_objc.swiftmodule %S/Inputs/def_basic_objc.sil
// RUN: llvm-bcanalyzer %t/def_basic_objc.swiftmodule | %FileCheck %s
// RUN: %target-build-swift -Xllvm -new-mangling-for-tests -Xfrontend %clang-importer-sdk -emit-silgen -Xfrontend -sil-link-all -I %t %s | %FileCheck %S/Inputs/def_basic_objc.sil

// This test currently is written such that no optimizations are assumed.
// REQUIRES: swift_test_mode_optimize_none
// REQUIRES: objc_interop

// CHECK-NOT: UnknownCode

import def_basic_objc

func test_all() {
  serialize_all()
}

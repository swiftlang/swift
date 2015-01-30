// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: %target-build-swift -Xfrontend %clang-importer-sdk -I %S/../Inputs/clang-importer-sdk/swift-modules -emit-module -Xfrontend -disable-diagnostic-passes -Xfrontend -sil-serialize-all -force-single-frontend-invocation -o %t/def_basic.swiftmodule %S/Inputs/def_basic.sil
// RUN: llvm-bcanalyzer %t/def_basic.swiftmodule | FileCheck %s
// RUN: %target-build-swift -Xfrontend %clang-importer-sdk -emit-silgen -Xfrontend -sil-link-all -I %t %s | FileCheck %S/Inputs/def_basic.sil

// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: %target-build-swift -Xfrontend %clang-importer-sdk -emit-module -Xfrontend -disable-diagnostic-passes -force-single-frontend-invocation -Xfrontend -sil-serialize-all -o %t/def_basic.swiftmodule %S/Inputs/def_basic.sil
// RUN: %target-build-swift -Xfrontend %clang-importer-sdk -emit-silgen -Xfrontend -sil-link-all -I %t %s | FileCheck -check-prefix=CHECK_DECL %S/Inputs/def_basic.sil

// XFAIL: linux

// CHECK-NOT: UnknownCode

// Inputs/def_basic.sil is based on basic.sil under test/SIL/Parser.

import def_basic

func test_all() {
  serialize_all()
}

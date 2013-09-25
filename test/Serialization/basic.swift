// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: %swift -emit-module -sil-serialize-all -o %t %S/Inputs/def_basic.sil
// RUN: llvm-bcanalyzer %t/def_basic.swiftmodule | FileCheck %s
// RUN: %swift -emit-silgen -sil-link-all -I=%t %s | FileCheck %S/Inputs/def_basic.sil

// CHECK-NOT: UnknownCode

// Inputs/def_basic.sil is based on basic.sil under test/SIL/Parser.

import def_basic

func test_all() {
  serialize_all()
}

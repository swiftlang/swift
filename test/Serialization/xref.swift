// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: %swift -emit-module -o %t/alias.swiftmodule %S/Inputs/alias.swift
// RUN: %swift -emit-module -o %t/has_xref.swiftmodule %S/Inputs/has_xref.swift
// RUN: llvm-bcanalyzer %t/has_xref.swiftmodule | FileCheck %s
// RUN: %swift -emit-sil -I=%t %s > /dev/null

// CHECK-NOT: FALL_BACK_TO_TRANSLATION_UNIT

import has_xref

numeric(42)
conditional(true)

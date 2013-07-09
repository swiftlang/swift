// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: %swift -emit-module -o %t/has_array.swiftmodule %S/Inputs/has_array.swift
// RUN: llvm-bcanalyzer %t/has_array.swiftmodule | FileCheck %s
// RUN: %swift -emit-silgen -I=%t %s -o /dev/null
// XFAIL: *

// CHECK-NOT: FALL_BACK_TO_TRANSLATION_UNIT

import has_array

fourByFour[3][3] = 42

// NOTE: Do not add anything else to this test. It is intended to be a bare
// minimum test for serializing ArrayType that should start passing as soon as
// Parse, Sema, and SILGen can handle ArrayTypes.

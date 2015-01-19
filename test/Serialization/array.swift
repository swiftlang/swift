// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: %target-swift-frontend -emit-module -o %t %S/Inputs/has_array.swift
// RUN: llvm-bcanalyzer %t/has_array.swiftmodule | FileCheck %s
// RUN: %target-swift-frontend -emit-silgen -I %t %s -o /dev/null
// XFAIL: *

// CHECK-NOT: UnknownCode

import has_array

fourByFour[3][3] = 42

// NOTE: Do not add anything else to this test. It is intended to be a bare
// minimum test for serializing ArrayType that should start passing as soon as
// Parse, Sema, and SILGen can handle ArrayTypes.

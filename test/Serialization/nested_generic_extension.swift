// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: %target-swift-frontend -emit-module -o %t %S/Inputs/has_nested_generic_extension.swift
// RUN: llvm-bcanalyzer %t/has_nested_generic_extension.swiftmodule | %FileCheck %s
// RUN: %target-swift-frontend -emit-ir -I %t %s -o /dev/null

// CHECK-NOT: UnknownCode

import has_nested_generic_extension

var sillyGeneric = Outer<String>.Inner<Float>()
let result: Float = sillyGeneric.extensionMethod(t: "square root of two")

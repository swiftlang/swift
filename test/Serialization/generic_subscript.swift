// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: %target-swift-frontend -emit-module -o %t %S/Inputs/has_generic_subscript.swift
// RUN: llvm-bcanalyzer %t/has_generic_subscript.swiftmodule | %FileCheck %s
// RUN: %target-swift-frontend -emit-ir -I %t %s -o /dev/null

// CHECK-NOT: UnknownCode

import has_generic_subscript

var sillyDict = GenericSubscript()
var value: Int = sillyDict["beer"]

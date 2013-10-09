// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: %swift -std=axle -emit-module -o %t %S/Inputs/def_axle.swift
// RUN: llvm-bcanalyzer %t/def_axle.swiftmodule | FileCheck %s
// RUN: %swift -std=axle -parse -I=%t %s -o /dev/null

// CHECK-NOT: UnknownCode
import def_axle

v4f = Vec<Float, 4>(1.5, 2.5, 3.5, 4.5)

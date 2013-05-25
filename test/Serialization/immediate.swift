// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: %swift -emit-module -o %t/alias.swiftmodule %S/Inputs/alias.swift
// RUN: %swift -I=%t -i %s | FileCheck %s

import alias

var i : MyInt64
i = Int64(42).value
var j : AnotherInt64 = i
print("\(Int64(j))\n")

// CHECK: 42

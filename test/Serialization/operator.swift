// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: %swift -emit-module -o %t/def_operator.swiftmodule %S/Inputs/def_operator.swift
// RUN: llvm-bcanalyzer %t/def_operator.swiftmodule | FileCheck %s
// RUN: %swift -i -I=%t %s | FileCheck --check-prefix=OUTPUT %s

// CHECK-NOT: FALL_BACK_TO_TRANSLATION_UNIT

import def_operator

func [prefix] ~~~(x : Int) -> (Int, Int, Int) {
  return (x, x, x)
}

var triple = (~~~42)
println("(\(triple.0), \(triple.1), \(triple.2))")
// OUTPUT: (42, 42, 42)

func [postfix] ^^(x : Int) -> Int {
  return x ^ x
}

println("\(1^^)")
// OUTPUT: 0

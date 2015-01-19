// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: %target-swift-frontend -emit-module %S/Inputs/sil_witness_tables_external_input.swift -o %t/Swift.swiftmodule -parse-stdlib -parse-as-library -module-name Swift -sil-serialize-all -module-link-name swiftCore
// RUN: %target-swift-frontend -O -sil-inline-threshold 0 -I %t %s -emit-sil | FileCheck %s

import Swift

// Make sure the specializer produces an external witness table.
//
// CHECK: sil_witness_table public_external [fragile] X: P module Swift {

func doSomething<T : P>(t : T) -> Y {
  return t.doSomething()
}

func done() -> Y {
  var x = X()
  return doSomething(x)
}

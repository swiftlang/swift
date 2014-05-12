// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: %swift -emit-module %S/Inputs/sil_witness_tables_external_input.swift -o %t/Swift.swiftmodule -parse-stdlib -parse-as-library -module-name Swift -sil-serialize-all -module-link-name swift_stdlib_core
// RUN: %swift -I=%t %s -emit-ir | FileCheck %s

import Swift

// CHECK: @_TMdVSs1X = external global %swift.full_type
// CHECK: @_TWPVSs1XSs1P = external global i8*

func doSomething<T : P>(t : T) -> Y {
  return t.doSomething()
}

func done() -> Y {
  var x = X()
  return doSomething(x)
}

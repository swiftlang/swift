// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module %S/Inputs/sil_witness_tables_external_input.swift -o %t/Swift.swiftmodule -parse-stdlib -parse-as-library -module-name Swift -module-link-name swiftCore
// RUN: %target-swift-frontend -O -I %t %s -Xllvm -sil-disable-pass=late-deadfuncelim -emit-sil | %FileCheck %s
// RUN: %target-swift-frontend -O -I %t %s -emit-sil | %FileCheck -check-prefix=CHECK-DFE %s

import Swift

// Make sure the specializer produces an external witness table.
//
// CHECK: sil_witness_table public_external X: P module Swift {

// Also check that late dead-function-elimination is removing externally
// available witness tables.
//
// CHECK-DFE-NOT: sil_witness_table public_external

func doSomething<T : P>(_ t : T) -> Y {
  return t.doSomething()
}

func done() -> Y {
  let x = X()
  return doSomething(x)
}

// Make sure dead function elimination does not get rid of done and doSomething.
done()

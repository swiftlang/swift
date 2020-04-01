// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -sil-inline-threshold 0 %S/Inputs/init_existential_inst_deserializes_witness_tables_input.swift -o %t/Swift.swiftmodule -emit-module -parse-as-library -parse-stdlib -module-link-name swiftCore -module-name Swift -O
// RUN: %target-swift-frontend -I %t -O %s -Xllvm -sil-disable-pass=late-deadfuncelim -emit-sil -o - | %FileCheck %s

// CHECK: sil_witness_table public_external X: P module Swift {

import Swift

var x = X()
whatShouldIDo(x)

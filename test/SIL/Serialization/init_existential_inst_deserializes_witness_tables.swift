// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: %swift -sil-inline-threshold 0 %S/Inputs/init_existential_inst_deserializes_witness_tables_input.swift -o %t/Swift.swiftmodule -emit-module -parse-as-library -parse-stdlib -module-link-name swift_stdlib_core -module-name Swift -sil-serialize-all -O3
// RUN: %swift -I=%t -O3 %s -emit-sil -o - | FileCheck %s

// CHECK: sil_witness_table public_external X: P module Swift {

import Swift

var x = X()
whatShouldIDo(x)

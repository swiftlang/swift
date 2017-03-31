// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: %target-swift-frontend -sil-inline-threshold 0 %S/Inputs/init_existential_inst_deserializes_witness_tables_input.swift -o %t/Swift.swiftmodule -emit-module -parse-as-library -parse-stdlib -module-link-name swiftCore -module-name Swift -sil-serialize-all -O
// RUN: %target-swift-frontend -I %t -O %s -emit-sil -o - | %FileCheck %s

// CHECK: sil_witness_table public_external [serialized] X: P module Swift {

import Swift

var x = X()
whatShouldIDo(x)

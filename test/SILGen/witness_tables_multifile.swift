// RUN: %target-swift-frontend -emit-silgen -primary-file %s %S/Inputs/witness_tables_multifile_2.swift | FileCheck %s -check-prefix=CHECK-FIRST-FILE
// RUN: %target-swift-frontend -emit-silgen %s -primary-file %S/Inputs/witness_tables_multifile_2.swift | FileCheck %s -check-prefix=CHECK-SECOND-FILE


protocol InheritsFooable : Fooable {}

struct FooStruct : InheritsFooable {}

// CHECK-FIRST-FILE-NOT: sil_witness_table hidden FooStruct: Fooable module witness_tables_multifile
// CHECK-FIRST-FILE: sil_witness_table hidden FooStruct: InheritsFooable module witness_tables_multifile
// CHECK-FIRST-FILE-NOT: sil_witness_table hidden FooStruct: Fooable module witness_tables_multifile

// CHECK-SECOND-FILE-NOT: sil_witness_table hidden FooStruct: InheritsFooable module witness_tables_multifile
// CHECK-SECOND-FILE: sil_witness_table hidden FooStruct: Fooable module witness_tables_multifile
// CHECK-SECOND-FILE-NOT: sil_witness_table hidden FooStruct: InheritsFooable module witness_tables_multifile

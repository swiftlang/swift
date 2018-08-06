// RUN: %target-swift-emit-silgen -enable-sil-ownership -primary-file %s %S/Inputs/witness_tables_multifile_2.swift | %FileCheck %s -allow-deprecated-dag-overlap -check-prefix=CHECK-FIRST-FILE
// RUN: %target-swift-emit-silgen -enable-sil-ownership %s -primary-file %S/Inputs/witness_tables_multifile_2.swift | %FileCheck %S/Inputs/witness_tables_multifile_2.swift -check-prefix=CHECK-SECOND-FILE


protocol InheritsFooable : Fooable {}

// CHECK-FIRST-FILE-NOT: sil_witness_table hidden FooStruct: Fooable module witness_tables_multifile
// CHECK-FIRST-FILE-DAG: sil_witness_table hidden FooStruct: InheritsFooable module witness_tables_multifile
// CHECK-FIRST-FILE-NOT: sil_witness_table hidden FooStruct: Fooable module witness_tables_multifile
struct FooStruct : InheritsFooable {}

// CHECK-FIRST-FILE-DAG: sil_witness_table hidden FooStruct2: Fooable module witness_tables_multifile
// CHECK-FIRST-FILE-DAG: sil_witness_table hidden FooStruct2: InheritsFooable module witness_tables_multifile
// CHECK-FIRST-FILE-NOT: sil_witness_table hidden FooStruct2: InheritsFooable2 module witness_tables_multifile
struct FooStruct2 : InheritsFooable { }

// CHECK-FIRST-FILE-DAG: sil_witness_table hidden FooStruct3: InheritsFooable2 module witness_tables_multifile
// CHECK-FIRST-FILE-DAG: sil_witness_table hidden FooStruct3: Fooable module witness_tables_multifile
// CHECK-FIRST-FILE-DAG: sil_witness_table hidden FooStruct3: InheritsFooable2 module witness_tables_multifile
struct FooStruct3 : InheritsFooable { }
extension FooStruct3 : InheritsFooable2 {
	func foo() { }
}

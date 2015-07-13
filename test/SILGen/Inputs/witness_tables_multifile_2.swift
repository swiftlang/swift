protocol Fooable {
	func foo()
}

protocol InheritsFooable2 : Fooable {}

// CHECK-SECOND-FILE-NOT: sil_witness_table hidden FooStruct: InheritsFooable module witness_tables_multifile
// CHECK-SECOND-FILE-DAG: sil_witness_table hidden FooStruct: Fooable module witness_tables_multifile
// CHECK-SECOND-FILE-NOT: sil_witness_table hidden FooStruct: InheritsFooable module witness_tables_multifile
extension FooStruct : Fooable {
	func foo() { }
}

// CHECK-SECOND-FILE-NOT: sil_witness_table hidden FooStruct2: InheritsFooable module witness_tables_multifile
// CHECK-SECOND-FILE-NOT: sil_witness_table hidden FooStruct2: Fooable module witness_tables_multifile
// CHECK-SECOND-FILE-DAG: sil_witness_table hidden FooStruct2: InheritsFooable2 module witness_tables_multifile
extension FooStruct2 : InheritsFooable2 {
	func foo() { }
}

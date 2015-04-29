// RUN: %target-swift-frontend %s -emit-ir -verify -g -o - | FileCheck %s

class C<A> {
// CHECK-DAG: ![[A:[0-9]+]] = !DICompositeType(tag: DW_TAG_structure_type, name: "_TtQq_C11archetypes21C"
// CHECK-DAG: ![[B:[0-9]+]] = !DICompositeType(tag: DW_TAG_structure_type, name: "_TtQq_FC11archetypes21C3foo
// CHECK-DAG: !DILocalVariable(tag: DW_TAG_arg_variable, name: "x", {{.*}}line: [[@LINE+2]],{{.*}}type: ![[A]]
// CHECK-DAG: !DILocalVariable(tag: DW_TAG_arg_variable, name: "y", {{.*}}line: [[@LINE+1]],{{.*}}type: ![[B]]
	func foo <B> (var x : A, var y : B)
	{
		println("hello world")
	}
}

C<Int>().foo(1, y: 3.14);

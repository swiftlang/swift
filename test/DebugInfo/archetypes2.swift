// RUN: %swift -triple x86_64-apple-darwin14 %s -emit-llvm -verify -g -o - | FileCheck %s
class C<A> {
// CHECK: ![[A:.*]] = {{.*}} ; [ DW_TAG_structure_type ] [_TtQq_C11archetypes21C]
// CHECK: ![[B:.*]] = {{.*}} ; [ DW_TAG_structure_type ] [_TtQq_FC11archetypes21C3fooU__FGS0_Q__U__FT1xQd__1yQ__T_]
// CHECK: metadata ![[A]], i32 0, i32 0} ; [ DW_TAG_arg_variable ] [x] [line [[@LINE+2]]]
// CHECK: metadata ![[B]], i32 0, i32 0} ; [ DW_TAG_arg_variable ] [y] [line [[@LINE+1]]]
	func foo <B> (x : A, y : B)
	{
		println("hello world")
	}
}

C<Int>().foo(1,3.14);

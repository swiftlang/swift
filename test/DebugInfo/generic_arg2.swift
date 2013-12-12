// RUN: %swift -triple x86_64-apple-darwin13 %s -emit-llvm -g -o - | FileCheck %s
// CHECK: @_TC12generic_arg25Class3fooU__fGS0_Q__U__FT1xQd__1yQ__T_{{.*}}, %swift.type* %U
// CHECK: %[[Y:.*]] = call %swift.opaque* %allocateBuffer3([24 x i8]* %{{.*}}, %swift.type* %U) #1
// CHECK: call void @llvm.dbg.value(metadata !{%swift.opaque* %[[Y]]}, i64 0, metadata ![[U:.*]]), !dbg
// Make sure there is no conflicting dbg.declare for this variable.x
// CHECK-NOT: dbg.declare{{.*}}metadata ![[U]]
class Class <T> {
// CHECK: ![[U]] ={{.*}} [ DW_TAG_arg_variable ] [y] [line [[@LINE+1]]]
	func foo <U> (x : T, y : U)
	{
		println("hello world")
	}

	func bar (x : String, y : Int) {
		println("hello world")
	}

	init() {}
}

func main() {
	var object : Class<String> = Class()
	var x = "hello"
	var y = 1234
	object.bar(x,y)
	object.foo(x,y)
}

main()

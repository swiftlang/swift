// RUN: %swift -enable-dynamic-value-type-layout -triple x86_64-apple-darwin13 %s -emit-llvm -g -o - | FileCheck %s
class Class <T> {
	var x : T

	init(_x : T) {x = _x}

  // Verify that the mangling of the decl context of the type U is correct.
  // CHECK: [ DW_TAG_structure_type ] [_TtQq_FC14dynamic_layout5Class3fooU__FGS0_Q__U__FT1yQ__TQd__Q__]
	func foo <U> (y : U) -> (T,U) {
		var tuple = (x,y)
		return tuple
	}
}

func main() {
	var v = Class<Int>(1)
	var tuple = v.foo("hi")
	println(tuple.0)
	println(tuple.1)
}

main()

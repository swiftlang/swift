// RUN: %target-swift-frontend -enable-dynamic-value-type-layout %s -emit-ir -g -o - | FileCheck %s

class Class <T> {
	var x : T

	init(_x : T) {x = _x}

  // Verify that the mangling of the decl context of the type U is correct.
  // CHECK: [ DW_TAG_structure_type ] [_TtQq_FC14dynamic_layout5Class3foo{{.*}}]
	func foo <U> (y : U) -> (T,U) {
		var tuple = (x,y)
		return tuple
	}
}

func main() {
	var v = Class<Int>(_x: 1)
	var tuple = v.foo("hi")
	println(tuple.0)
	println(tuple.1)
}

main()

// RUN: %target-swift-frontend %s -emit-ir -g -o - | FileCheck %s

// FIXME: Should be DW_TAG_interface_type
// CHECK: !DICompositeType(tag: DW_TAG_structure_type, name: "IGiveOutInts"
// CHECK-SAME:             identifier: [[PT:"[^"]+"]]
protocol IGiveOutInts {
	func callMe() -> Int
}

class SomeImplementor : IGiveOutInts {
        init() {} 
	func callMe() -> Int { return 1 }
}

class AFancierImplementor : IGiveOutInts {
	var myInt : Int
	init() {
		myInt = 1
	}

	func callMe() -> Int {
		myInt = myInt + 1
		return myInt
	}
}

func printSomeNumbers(var gen: IGiveOutInts) {
  // CHECK: !DILocalVariable(tag: DW_TAG_arg_variable, name: "gen",
  // CHECK-SAME:             line: [[@LINE-2]]
  // CHECK-SAME:             type: ![[PT]]
	var i = 1
	while i < 3 {
		println("\(gen.callMe())")
		i++
	}
}

var i1 : IGiveOutInts = SomeImplementor()
var i2 : IGiveOutInts = AFancierImplementor()

printSomeNumbers(i1)
printSomeNumbers(i2)


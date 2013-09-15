// RUN: %swift -triple x86_64-apple-darwin10 %s -emit-llvm -g -o - | FileCheck %s
// FIXME: Should be DW_TAG_interface_type
// CHECK: ![[PT:.*]] ={{.*}}; [ DW_TAG_base_type ] [_TtP11protocolarg12IGiveOutInts_]
protocol IGiveOutInts {
	func callMe() -> Int
}

class SomeImplementor : IGiveOutInts {
	func callMe() -> Int { return 1 }
}

class AFancierImplementor : IGiveOutInts {
	var myInt : Int
	constructor() {
		myInt = 1
	}

	func callMe() -> Int {
		myInt = myInt + 1
		return myInt
	}
}

func printSomeNumbers (gen : IGiveOutInts) {
        // CHECK: metadata !"gen", metadata !{{.*}}, i32 {{.*}}, metadata ![[PT]], i32 0, i32 0} ; [ DW_TAG_arg_variable ] [gen] [line [[@LINE-1]]]
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


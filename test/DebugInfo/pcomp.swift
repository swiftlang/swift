// RUN: %target-swift-frontend -primary-file %s -emit-ir -g -o - | FileCheck %s

protocol A {
	func x()
}

protocol B {
	func y()
}

// CHECK-DAG: _TtP5pcomp1AS_1B_
func f(arg :protocol<A,B>) {
}



protocol SomeProto {
	func f() -> Int
}

class SomeClass : SomeProto {
	func f() -> Int { return 1 }
}

class SomeOtherClass : SomeClass {
	override func f() -> Int { return 1 }
}
// This is an indirect value.
// CHECK-DAG: \0030{{.*}}!"_TtP5pcomp9SomeProto_"} ; [ DW_TAG_structure_type ] [SomeProto]
func main() {
	var p : SomeProto = SomeOtherClass()
	println("\(p.f())")
}


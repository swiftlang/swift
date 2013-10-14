// RUN: %swift -triple x86_64-apple-darwin10 %s -emit-llvm -g -o - | FileCheck %s
class AClass {
	func f () -> Int { return 1; }
}

class AnotherClass : AClass {
	func f() -> Int { return 2; }
}

struct AStruct {
	func f() -> Int { return 3; }
}

func app() {
	var ac : AClass = AnotherClass()
        // No members? No storage! Emitted as a constant 0, because.
        // CHECK: [ DW_TAG_variable ] [at] [line [[@LINE+1]]] [local] [def]
	var at = AStruct()
	println("\(ac.f()) \(at.f())")
}

app()

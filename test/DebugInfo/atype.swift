// RUN: %swift -triple x86_64-apple-darwin13 %s -emit-llvm -g -o - | FileCheck %s
class Class {
// CHECK: _TtQq_FC5atype5Class8functionFS0_U__FT1xQ__T_
	func function <T> (x : T) {
		println("hello world")
	}
}

func main() {
	var v = 1
	var c = Class()
	c.function(1)
}

main()

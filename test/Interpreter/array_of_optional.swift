// RUN: %swift -i %s | FileCheck %s
// REQUIRES: swift_interpreter

// <rdar://problem/15609900>

// CHECK: 10
// CHECK: None
// CHECK: 20
// CHECK: None
// CHECK: 30
// CHECK: hello world
func main() {
	var arrOpt : (Int?)[] = [10,.None,20,.None,30]
	for item in arrOpt {
		switch item {
			case .None:
			println("None")
			case .Some(var v):
			println(v)
		}
	}
	println("hello world")
}

main()



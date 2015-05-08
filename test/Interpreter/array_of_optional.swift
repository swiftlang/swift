// RUN: %target-run-simple-swift | FileCheck %s

// <rdar://problem/15609900>

// CHECK: 10
// CHECK: None
// CHECK: 20
// CHECK: None
// CHECK: 30
// CHECK: hello world
func main() {
	var arrOpt : [Int?] = [10,.None,20,.None,30]
	for item in arrOpt {
		switch item {
			case .None:
			print("None")
			case .Some(var v):
			print(v)
		}
	}
	print("hello world")
}

main()



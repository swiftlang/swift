// RUN: %target-run-simple-swift | %FileCheck %s
// REQUIRES: executable_test

// <rdar://problem/15609900>

// CHECK: 10
// CHECK: none
// CHECK: 20
// CHECK: none
// CHECK: 30
// CHECK: hello world
func main() {
	var arrOpt : [Int?] = [10,.none,20,.none,30]
	for item in arrOpt {
		switch item {
			case .none:
			print("none")
			case .some(var v):
			print(v)
		}
	}
	print("hello world")
}

main()



// RUN: %target-run-simple-swift | %FileCheck %s

// REQUIRES: executable_test

@_silgen_name("start")
func start() {
  print("init C")
}
@_silgen_name("barrier")
func barrier() {
	print("nothing uses C anymore")
}
@_silgen_name("end")
func end() {
  print("deinit C")
}

@_eagerMove class C {
	init() { start() }
	deinit { end() }
}

@_silgen_name("doit")
public func main() {
	C()
  barrier()
}

main()

// CHECK: init C
// CHECK: deinit C
// CHECK: nothing uses C anymore

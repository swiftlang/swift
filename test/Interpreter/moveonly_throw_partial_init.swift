// RUN: %target-run-simple-swift
// REQUIRES: executable_test

struct Butt: Error {}

struct Inside: ~Copyable {
	init() throws { throw Butt() }
}

class C {
	deinit { print("destroying") }
}

struct Outside: ~Copyable {
    let instance: Inside
	var c: C

    init() throws {
		c = C()
        let instance = try Inside()
        self.instance = instance
    }
}

// CHECK: begin
// CHECK-NEXT: destroying
// CHECK-NEXT: caught
// CHECK-NEXT: end

func test() {
	print("begin")
	do {
		_ = try Outside()
	} catch {
		print("caught")
	}
	print("end")
}
test()


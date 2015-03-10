// RUN: rm -rf %t && mkdir -p %t
// RUN: %target-swift-frontend -c %S/Inputs/multithread_module/main.swift -o %t/main.o %s -o %t/mt_module.o -num-threads 2 -O -module-name test -Xllvm -enable-static-init 
// RUN: %target-swiftc_driver %t/main.o %t/mt_module.o -o %t/a.out
// RUN: %target-run %t/a.out | FileCheck %s


// Test compilation of a module in multi-threaded compilation.
// The main purpose of the test is to check that the generated LLVM modules are not corrupt
// and that linking succeeds.
// Note that -enable-static-init is specified to test the multi-threaded specific handling of static initializers.

// CHECK: 28
// CHECK: 125
// CHECK: 42
// CHECK: 237

public func testit(x: Int) -> Int {
	return incrementit(x)
}

public class Base {
	func memberfunc(x: Int) -> Int {
		return x + 1
	}
}

public var g2 = 123

@inline(never)
func callmember(b: Base) -> Int {
	return b.memberfunc(g2)
}

@inline(never)
private func privateInc(x: Int) -> Int {
	return x + 3
}

func callPrivInc(x: Int) -> Int {
	return privateInc(x)
}

protocol MyProto {
	func protofunc() -> Int
}

@inline(never)
func callproto(p: MyProto) {
	println(p.protofunc())
}



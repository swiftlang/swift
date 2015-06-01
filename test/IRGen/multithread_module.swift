// Also run this test in optimize test modes.
// REQUIRES: optimize_test

// RUN: rm -rf %t && mkdir -p %t

// RUN: %target-swift-frontend %S/Inputs/multithread_module/main.swift -emit-ir -o %t/main.ll %s -o %t/mt_module.ll -num-threads 2 -O -g -module-name test
// RUN: FileCheck --check-prefix=CHECK-MAINLL %s <%t/main.ll
// RUN: FileCheck --check-prefix=CHECK-MODULELL %s <%t/mt_module.ll

// RUN: %target-swift-frontend -c %S/Inputs/multithread_module/main.swift -o %t/main.o %s -o %t/mt_module.o -num-threads 2 -O -g -module-name test
// RUN: %target-build-swift %t/main.o %t/mt_module.o -o %t/a.out
// RUN: %target-run %t/a.out | FileCheck %s


// Test compilation of a module in multi-threaded compilation.
// The main purpose of the test is to check that the generated LLVM modules are not corrupt
// and that linking succeeds.

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
	print(p.protofunc())
}

// Check the llvm IR files:

// Check if the DI filename is correct and not "<unknown>".

// CHECK-MAINLL: DICompileUnit{{.*}} file: [[F:![0-9]+]]
// CHECK-MAINLL: [[F]] = !DIFile(filename: "main.swift", directory: "{{.*}}IRGen/Inputs/multithread_module")

// CHECK-MODULELL: DICompileUnit{{.*}} file: [[F:![0-9]+]]
// CHECK-MODULELL: [[F]] = !DIFile(filename: "multithread_module.swift", directory: "{{.*}}IRGen")

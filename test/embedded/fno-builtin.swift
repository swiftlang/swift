// RUN: %target-swift-emit-ir %s %S/Inputs/print.swift -module-name main -Xcc -fno-builtin -enable-experimental-feature Embedded | %FileCheck %s
// RUN: %target-swift-emit-ir %s %S/Inputs/print.swift -module-name main -Xcc -ffreestanding -enable-experimental-feature Embedded | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: optimized_stdlib
// REQUIRES: VENDOR=apple
// REQUIRES: OS=macosx

public func foo() -> [Int] {
	var a = [1, 2, 3]
	a.append(4)
	let b = a.sorted()
	return b
}

// CHECK: define {{.*}}@"$s4main3fooSaySiGyF"()

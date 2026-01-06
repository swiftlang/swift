// RUN: %target-swift-emit-ir %s -parse-as-library -module-name main -Xcc -fno-builtin -enable-experimental-feature Embedded | %FileCheck %s
// RUN: %target-swift-emit-ir %s -parse-as-library -module-name main -Xcc -ffreestanding -enable-experimental-feature Embedded | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: optimized_stdlib
// REQUIRES: swift_feature_Embedded

public func foo() -> [Int] {
	var a = [1, 2, 3]
	a.append(4)
	let b = a.sorted()
	return b
}

// CHECK: define {{.*}}@"$e4main3fooSaySiGyF"()

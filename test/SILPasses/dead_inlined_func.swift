// RUN: %swift -O -g %s -emit-sil | FileCheck %s -check-prefix=CHECK-SIL
// RUN: %swift -O -g %s -sil-serialize-all -emit-ir | FileCheck %s -check-prefix=CHECK-IR


// The dead inlined function should not be in the SIL
// CHECK-SIL-NOT: sil {{.*}}to_be_inlined


// ... and also not in the llvm IR
// CHECK-IR-NOT: define {{.*}}to_be_inlined

// CHECK-IR: metadata !{metadata !"{{.*}}to_be_inlined{{.*}}"

private func to_be_inlined(x: Int) -> Int {
	return x + 1
}

public func caller(x: Int) -> Int {
	return to_be_inlined(x)
}


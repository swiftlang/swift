// RUN: %target-swift-frontend -O -emit-sil %s %S/Inputs/internal_func.swift  | FileCheck %s
// RUN: %target-swift-frontend -O -enable-testing -emit-sil %s %S/Inputs/internal_func.swift  | FileCheck -check-prefix=CHECK-TESTABLE %s

// Check that an internal function from another file is removed after it gets
// dead through inlining.

// CHECK-NOT: sil {{.*}}testfunc
// CHECK-TESTABLE: sil {{.*}}testfunc

public func caller_func() {
	testfunc()
}


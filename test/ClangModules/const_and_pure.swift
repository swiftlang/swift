// RUN: %target-swift-frontend -emit-sil %s -import-objc-header %S/Inputs/const_and_pure.h | FileCheck %s

func testit() {
	const_function()

	pure_function()

	normal_function()
}

// CHECK: sil [readnone] @const_function : $@convention(c) () -> ()
// CHECK: sil [readonly] @pure_function : $@convention(c) () -> ()
// CHECK: sil @normal_function : $@convention(c) () -> ()



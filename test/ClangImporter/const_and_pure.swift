// RUN: %target-swift-frontend -emit-sil %s -enable-objc-interop -import-objc-header %S/Inputs/const_and_pure.h | %FileCheck %s

func testit() {
	const_function()

	pure_function()

	normal_function()
}

// CHECK: sil [readnone] [clang const_function] @const_function : $@convention(c) () -> ()
// CHECK: sil [readonly] [clang pure_function] @pure_function : $@convention(c) () -> ()
// CHECK: sil [clang normal_function] @normal_function : $@convention(c) () -> ()



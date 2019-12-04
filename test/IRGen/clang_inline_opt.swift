// RUN: %target-swift-frontend -enable-objc-interop -import-objc-header %S/Inputs/c_functions.h -primary-file %s -O -emit-ir -disable-llvm-optzns | %FileCheck %s

func return10() -> UInt32 {
	return return7() + 3
}

// Sanity check that we tell Clang to generate optimizable code when
// we're optimizing.

// CHECK: define internal{{(zeroext)?}} i32 @return7() [[CLANG_ATTRS:#[0-9]+]] {

// CHECK: attributes [[CLANG_ATTRS]] = {
// CHECK-NOT: noinline
// CHECK-SAME: }

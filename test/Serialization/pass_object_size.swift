// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t/PassObjectSizeLib.swiftmodule -module-name PassObjectSizeLib -import-objc-header %S/Inputs/pass_object_size.h %S/Inputs/pass_object_size_lib.swift
// RUN: %target-swift-frontend -emit-ir -I %t -import-objc-header %S/Inputs/pass_object_size.h -primary-file %s | %FileCheck %s

// The pass_object_size flags live on SILParameterInfo, so they travel through
// the module format. Deserializing the transparent body inlined below has to
// bring them back, or the cross-module call loses its implicit size argument.

// REQUIRES: PTRSIZE=64

import PassObjectSizeLib

// CHECK-LABEL: define hidden swiftcc void @"$s16pass_object_size4testyySpys5Int32VGF"
func test(_ p: UnsafeMutablePointer<CInt>) {
  // CHECK: [[SIZE:%.*]] = call i64 @llvm.objectsize.i64.p0(ptr %0, i1 false, i1 true, i1 false)
  // CHECK: call void @pos_max(ptr %0, i64 [[SIZE]])
  callPassObject(p)
}

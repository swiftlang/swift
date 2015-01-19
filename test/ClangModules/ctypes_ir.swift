// RUN: %target-swift-frontend %clang-importer-sdk -emit-ir -o - -primary-file %s | FileCheck %s

// REQUIRES: OS=macosx

import ctypes

// CHECK-LABEL: define hidden void @_TF9ctypes_ir9testColorFT_T_
func testColor() {
  // CHECK: call i32 @_TFSCg5greenVSC5Color()
  var c : Color = green
}

// CHECK-LABEL: define linkonce_odr hidden i32 @_TFSCg5greenVSC5Color
// CHECK: call i32 @_TFVSC5ColorCfMS_FVSs6UInt32S_(i32 1)

// CHECK-LABEL: define hidden void @_TF9ctypes_ir12testAnonEnumFT_T_
func testAnonEnum() {
  // CHECK: call i64 @_TFSCg10AnonConst2Su
  var a = AnonConst2
}

// CHECK-LABEL: define linkonce_odr hidden i64 @_TFSCg10AnonConst2Su
// CHECK-NOT: ret i64
// CHECK: ret i64 30064771073

// CHECK-LABEL: define hidden void @_TF9ctypes_ir17testAnonEnumSmallFT_T_
func testAnonEnumSmall() {
  // CHECK: call i64 @_TFSCg15AnonConstSmall2Si
  var a = AnonConstSmall2
}

// CHECK-LABEL: define linkonce_odr hidden i64 @_TFSCg15AnonConstSmall2Si
// CHECK-NOT: ret i64
// CHECK: ret i64 17

func testStructWithFlexibleArray(s : StructWithFlexibleArray)
{
  var a = s.a
}

// Make sure flexible array struct member isn't represented in IR function signature as i0 (or at all). rdar://problem/18510461
// CHECK-LABEL: define hidden void @_TF9ctypes_ir27testStructWithFlexibleArrayFVSC23StructWithFlexibleArrayT_(i32)

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-ir -o - -primary-file %s | FileCheck %s

// REQUIRES: OS=macosx

import ctypes

// CHECK-LABEL: define hidden void @_TF9ctypes_ir9testColorFT_T_
func testColor() {
  // CHECK: call i32 @_TFVSC5ColorCfMS_FVSs6UInt32S_(i32 1)
  var c : Color = green
}

// CHECK-LABEL: define hidden void @_TF9ctypes_ir12testAnonEnumFT_T_
func testAnonEnum() {
  // CHECK: store i64 30064771073
  var a = AnonConst2
}

// CHECK-LABEL: define hidden void @_TF9ctypes_ir17testAnonEnumSmallFT_T_
func testAnonEnumSmall() {
  // CHECK: store i64 17
  var a = AnonConstSmall2
}

func testStructWithFlexibleArray(s : StructWithFlexibleArray)
{
  var a = s.a
}

// Make sure flexible array struct member isn't represented in IR function signature as i0 (or at all). rdar://problem/18510461
// CHECK-LABEL: define hidden void @_TF9ctypes_ir27testStructWithFlexibleArrayFVSC23StructWithFlexibleArrayT_(i32)

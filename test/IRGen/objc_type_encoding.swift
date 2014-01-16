// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -triple x86_64-apple-darwin10 -module-cache-path=%t/clang-module-cache -sdk=%S/Inputs -I=%S/Inputs %s -emit-llvm | FileCheck %s

import Foundation
import gizmo

@objc class Methods {
  @objc func testSizedUnsignedTypes(a: UInt8) b(b: UInt16) c(c: UInt32) d(d: UInt64) {}
// CHECK: private unnamed_addr constant [20 x i8] c"v36@0:8C16S20I24Q28\00"

  @objc func testSizedFloats(a: Float32) b(b: Float64) {}
// CHECK: private unnamed_addr constant [14 x i8] c"v28@0:8f16d20\00"

  @objc func testParens(a: ((Int))) {}
// CHECK: private unnamed_addr constant [11 x i8] c"v24@0:8q16\00"

  @objc func testPrimitives(b: CBool) i(i: Int) f(f: Float) d(d: Double)
    -> COpaquePointer { return COpaquePointer() }
// CHECK: private unnamed_addr constant [21 x i8] c"^v40@0:8B16q20f28d32\00"

  @objc func testCSignedTypes(a: CSignedChar) b(b: CShort) c(c: CInt) d(d: CLong) e(e: CLongLong) {}
// CHECK: private unnamed_addr constant [23 x i8] c"v44@0:8c16s20i24q28q36\00"

  @objc func testCUnsignedTypes(a: CUnsignedChar) b(b: CUnsignedShort) c(c: CUnsignedInt) d(d: CUnsignedLong) e(e: CUnsignedLongLong) {}
// CHECK: private unnamed_addr constant [23 x i8] c"v44@0:8C16S20I24Q28Q36\00"

  @objc func testCChars(basic: CChar) wchar(wide: CWideChar) char16(char16: CChar16) char32(char32: CChar32) {}
// CHECK: private unnamed_addr constant [20 x i8] c"v32@0:8c16i20S24i28\00"

  @objc func testCBool(a: CBool) {}
// CHECK: private unnamed_addr constant [11 x i8] c"v20@0:8B16\00"

  @objc func testSizedSignedTypes(a: Int8) b(b: Int16) c(c: Int32) d(d: Int64) {}
// CHECK: private unnamed_addr constant [20 x i8] c"v36@0:8c16s20i24q28\00"
}

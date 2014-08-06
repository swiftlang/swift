// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -target x86_64-apple-macosx10.9 -module-cache-path %t/clang-module-cache -sdk %S/Inputs %s -emit-ir | FileCheck %s
import gizmo

// CHECK: define i64 @_TFC12clang_inline16CallStaticInline10ReturnZerofS0_FT_Si(%C12clang_inline16CallStaticInline*) {
// CHECK: define internal i32 @zero() #0 {
class CallStaticInline {
  func ReturnZero() -> Int { return Int(zero()) }
}

// CHECK: define i64 @_TFC12clang_inline17CallStaticInline210ReturnZerofS0_FT_Si(%C12clang_inline17CallStaticInline2*) {
// CHECK: define internal i32 @wrappedZero() #0 {
class CallStaticInline2 {
  func ReturnZero() -> Int { return Int(wrappedZero()) }
}

// CHECK: define i32 @_TF12clang_inline10testExternFT_VSs5Int32() {
// CHECK: define internal i32 @wrappedGetInt() #0 {
func testExtern() -> CInt {
  return wrappedGetInt()
}

// CHECK: define internal i32 @innerZero() #2 {
// CHECK: declare i32 @getInt() #3

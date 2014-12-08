// RUN: rm -rf %t && mkdir -p %t
// RUN: %swift -target x86_64-apple-macosx10.9 -sdk %S/Inputs -primary-file %s -emit-ir | FileCheck %s

// RUN: mkdir -p %t/Empty.framework/Modules/Empty.swiftmodule
// RUN: %swift -target x86_64-apple-macosx10.9 -emit-module-path %t/Empty.framework/Modules/Empty.swiftmodule/x86_64.swiftmodule %S/../Inputs/empty.swift -module-name Empty
// RUN: %swift -target x86_64-apple-macosx10.9 -sdk %S/Inputs -primary-file %s -F %t -DIMPORT_EMPTY -emit-ir | FileCheck %s

#if IMPORT_EMPTY
import Empty
#endif

import gizmo

// CHECK: define hidden i64 @_TFC12clang_inline16CallStaticInline10ReturnZerofS0_FT_Si(%C12clang_inline16CallStaticInline*) {
// CHECK: define internal i32 @zero() #0 {
class CallStaticInline {
  func ReturnZero() -> Int { return Int(zero()) }
}

// CHECK: define hidden i64 @_TFC12clang_inline17CallStaticInline210ReturnZerofS0_FT_Si(%C12clang_inline17CallStaticInline2*) {
// CHECK: define internal i32 @wrappedZero() #0 {
class CallStaticInline2 {
  func ReturnZero() -> Int { return Int(wrappedZero()) }
}

// CHECK: define hidden i32 @_TF12clang_inline10testExternFT_VSs5Int32() {
// CHECK: define internal i32 @wrappedGetInt() #0 {
func testExtern() -> CInt {
  return wrappedGetInt()
}

// CHECK: define hidden i32 @_TF12clang_inline16testAlwaysInlineFT_VSs5Int32() [[SSP:#[0-9]+]] {
// CHECK-NOT: @alwaysInlineNumber
// CHECK:   ret i32 17
// CHECK-NOT: @alwaysInlineNumber
func testAlwaysInline() -> CInt {
  return alwaysInlineNumber()
}

// CHECK-NOT: @alwaysInlineNumber
// CHECK: define internal i32 @innerZero() [[INNER_ZERO_ATTR:#[0-9]+]] {
// CHECK: declare i32 @getInt() [[GET_INT_ATTR:#[0-9]+]]
// CHECK-NOT: @alwaysInlineNumber

// CHECK: attributes [[SSP]] = { ssp }
// CHECK: attributes [[INNER_ZERO_ATTR]] = { inlinehint nounwind ssp 
// CHECK: attributes [[GET_INT_ATTR]] = {

// RUN: rm -rf %t && mkdir -p %t
// RUN: %target-swift-frontend -sdk %S/Inputs -primary-file %s -emit-ir | FileCheck %s

// RUN: mkdir -p %t/Empty.framework/Modules/Empty.swiftmodule
// RUN: %target-swift-frontend -emit-module-path %t/Empty.framework/Modules/Empty.swiftmodule/%target-swiftmodule-name %S/../Inputs/empty.swift -module-name Empty
// RUN: %target-swift-frontend -sdk %S/Inputs -primary-file %s -F %t -DIMPORT_EMPTY -emit-ir > %t.ll
// RUN: FileCheck %s < %t.ll
// RUN: FileCheck -check-prefix=NEGATIVE %s < %t.ll

// REQUIRES: CPU=i386_or_x86_64
// XFAIL: linux

#if IMPORT_EMPTY
import Empty
#endif

import gizmo

// CHECK-LABEL: define hidden i64 @_TFC12clang_inline16CallStaticInline10ReturnZerofS0_FT_VSs5Int64(%C12clang_inline16CallStaticInline*) {
class CallStaticInline {
  func ReturnZero() -> Int64 { return Int64(zero()) }
}

// CHECK-LABEL: define hidden i64 @_TFC12clang_inline17CallStaticInline210ReturnZerofS0_FT_VSs5Int64(%C12clang_inline17CallStaticInline2*) {
class CallStaticInline2 {
  func ReturnZero() -> Int64 { return Int64(wrappedZero()) }
}


// CHECK-LABEL: define hidden i32 @_TF12clang_inline10testExternFT_VSs5Int32() {
func testExtern() -> CInt {
  return wrappedGetInt()
}

// CHECK-LABEL: define hidden i32 @_TF12clang_inline16testAlwaysInlineFT_VSs5Int32()
// CHECK:       [[SSP:#[0-9]+]] {
// NEGATIVE-NOT: @alwaysInlineNumber
// CHECK:   ret i32 17
func testAlwaysInline() -> CInt {
  return alwaysInlineNumber()
}

// CHECK-LABEL: define hidden i32 @_TF12clang_inline20testInlineRedeclaredFT_VSs5Int32() {
func testInlineRedeclared() -> CInt {
  return zeroRedeclared()
}

// CHECK-LABEL: define hidden i32 @_TF12clang_inline27testInlineRedeclaredWrappedFT_VSs5Int32() {
func testInlineRedeclaredWrapped() -> CInt {
  return wrappedZeroRedeclared()
}

// CHECK-LABEL: define internal i32 @zero()
// CHECK:         [[INLINEHINT_SSP_UWTABLE:#[0-9]+]] {

// CHECK-LABEL: define internal i32 @wrappedZero()
// CHECK:         [[INLINEHINT_SSP_UWTABLE:#[0-9]+]] {

// CHECK-LABEL: define internal i32 @wrappedGetInt()
// CHECK:         [[INLINEHINT_SSP_UWTABLE:#[0-9]+]] {

// CHECK-LABEL: define internal i32 @zeroRedeclared() #{{[0-9]+}} {

// CHECK-LABEL: define internal i32 @wrappedZeroRedeclared() #{{[0-9]+}} {

// CHECK-LABEL: define internal i32 @innerZero()
// CHECK:         [[INNER_ZERO_ATTR:#[0-9]+]] {
// CHECK-LABEL: declare i32 @getInt()
// CHECK:         [[GET_INT_ATTR:#[0-9]+]]

// CHECK: attributes [[SSP]] = { ssp }
// CHECK: attributes [[INLINEHINT_SSP_UWTABLE]] = { inlinehint ssp {{.*}}}
// CHECK: attributes [[INNER_ZERO_ATTR]] = { inlinehint nounwind ssp 
// CHECK: attributes [[GET_INT_ATTR]] = {

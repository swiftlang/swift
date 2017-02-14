// These run lines use "-Xcc -O3" to trick Clang into emitting IR as /if/ we
// were going to optimize without actually running the optimizer. This lets us
// check that the static inline functions in Gizmo.h are correctly emitted as
// inlineable.

// RUN: rm -rf %t && mkdir -p %t
// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -assume-parsing-unqualified-ownership-sil -sdk %S/Inputs -primary-file %s -Xcc -O3 -emit-ir | %FileCheck %s

// RUN: mkdir -p %t/Empty.framework/Modules/Empty.swiftmodule
// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -assume-parsing-unqualified-ownership-sil -emit-module-path %t/Empty.framework/Modules/Empty.swiftmodule/%target-swiftmodule-name %S/../Inputs/empty.swift -module-name Empty
// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -assume-parsing-unqualified-ownership-sil -sdk %S/Inputs -primary-file %s -F %t -DIMPORT_EMPTY -Xcc -O3 -emit-ir > %t.ll
// RUN: %FileCheck %s < %t.ll
// RUN: %FileCheck -check-prefix=NEGATIVE %s < %t.ll

// REQUIRES: CPU=i386_or_x86_64
// XFAIL: linux

#if IMPORT_EMPTY
import Empty
#endif

import gizmo

// CHECK-LABEL: define hidden swiftcc i64 @_T012clang_inline16CallStaticInlineC10ReturnZeros5Int64VyF(%C12clang_inline16CallStaticInline* swiftself) {{.*}} {
class CallStaticInline {
  func ReturnZero() -> Int64 { return Int64(zero()) }
}

// CHECK-LABEL: define internal i32 @zero()
// CHECK:         [[INLINEHINT_SSP_UWTABLE:#[0-9]+]] {

// CHECK-LABEL: define hidden swiftcc i64 @_T012clang_inline17CallStaticInline2C10ReturnZeros5Int64VyF(%C12clang_inline17CallStaticInline2* swiftself) {{.*}} {
class CallStaticInline2 {
  func ReturnZero() -> Int64 { return Int64(wrappedZero()) }
}

// CHECK-LABEL: define internal i32 @wrappedZero()
// CHECK:         [[INLINEHINT_SSP_UWTABLE:#[0-9]+]] {

// CHECK-LABEL: define hidden swiftcc i32 @_T012clang_inline10testExterns5Int32VyF() {{.*}} {
func testExtern() -> CInt {
  return wrappedGetInt()
}

// CHECK-LABEL: define internal i32 @wrappedGetInt()
// CHECK:         [[INLINEHINT_SSP_UWTABLE:#[0-9]+]] {

// CHECK-LABEL: define hidden swiftcc i32 @_T012clang_inline16testAlwaysInlines5Int32VyF()
// CHECK:       [[SSP:#[0-9]+]] {
// NEGATIVE-NOT: @alwaysInlineNumber
// CHECK:   ret i32 17
func testAlwaysInline() -> CInt {
  return alwaysInlineNumber()
}

// CHECK-LABEL: define hidden swiftcc i32 @_T012clang_inline20testInlineRedeclareds5Int32VyF() {{.*}} {
func testInlineRedeclared() -> CInt {
  return zeroRedeclared()
}

// CHECK-LABEL: define internal i32 @zeroRedeclared() #{{[0-9]+}} {

// CHECK-LABEL: define hidden swiftcc i32 @_T012clang_inline27testInlineRedeclaredWrappeds5Int32VyF() {{.*}} {
func testInlineRedeclaredWrapped() -> CInt {
  return wrappedZeroRedeclared()
}

// CHECK-LABEL: define internal i32 @wrappedZeroRedeclared() #{{[0-9]+}} {

// CHECK-LABEL: define hidden swiftcc i32 @_T012clang_inline22testStaticButNotInlines5Int32VyF() {{.*}} {
func testStaticButNotInline() -> CInt {
  return staticButNotInline()
}

// CHECK-LABEL: define internal i32 @staticButNotInline() #{{[0-9]+}} {

// CHECK-LABEL: define internal i32 @innerZero()
// CHECK:         [[INNER_ZERO_ATTR:#[0-9]+]] {
// CHECK-LABEL: declare i32 @getInt()
// CHECK:         [[GET_INT_ATTR:#[0-9]+]]

// CHECK: attributes [[INLINEHINT_SSP_UWTABLE]] = { inlinehint ssp {{.*}}}
// CHECK: attributes [[SSP]] = { ssp {{.*}} }
// CHECK: attributes [[INNER_ZERO_ATTR]] = { inlinehint nounwind ssp 
// CHECK: attributes [[GET_INT_ATTR]] = {

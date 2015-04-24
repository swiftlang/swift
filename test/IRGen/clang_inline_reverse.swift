// Same test as clang_inline.swift, but with the order swapped.

// RUN: %target-swift-frontend -sdk %S/Inputs -primary-file %s -emit-ir -module-name clang_inline | FileCheck %s

// REQUIRES: CPU=i386_or_x86_64
// XFAIL: linux

import gizmo

// CHECK-LABEL: define hidden i64 @_TFC12clang_inline16CallStaticInline10ReturnZerofS0_FT_VSs5Int64(%C12clang_inline16CallStaticInline*) {
class CallStaticInline {
  func ReturnZero() -> Int64 { return Int64(wrappedZero()) }
}

// CHECK-LABEL: define internal i32 @wrappedZero() #0 {

// CHECK-LABEL: define hidden i64 @_TFC12clang_inline17CallStaticInline210ReturnZerofS0_FT_VSs5Int64(%C12clang_inline17CallStaticInline2*) {
class CallStaticInline2 {
  func ReturnZero() -> Int64 { return Int64(zero()) }
}

// CHECK-LABEL: define internal i32 @zero() #0 {

// CHECK-LABEL: define internal i32 @innerZero() #2 {

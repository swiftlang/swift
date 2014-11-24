// Same test as clang_inline.swift, but with the order swapped.

// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -target x86_64-apple-macosx10.9 -module-cache-path %t/clang-module-cache -sdk %S/Inputs -primary-file %s -emit-ir -module-name clang_inline | FileCheck %s
// XFAIL: linux
import gizmo

// CHECK: define hidden i64 @_TFC12clang_inline16CallStaticInline10ReturnZerofS0_FT_Si(%C12clang_inline16CallStaticInline*) {
// CHECK: define internal i32 @wrappedZero() #0 {
class CallStaticInline {
  func ReturnZero() -> Int { return Int(wrappedZero()) }
}

// CHECK: define hidden i64 @_TFC12clang_inline17CallStaticInline210ReturnZerofS0_FT_Si(%C12clang_inline17CallStaticInline2*) {
// CHECK: define internal i32 @zero() #0 {
class CallStaticInline2 {
  func ReturnZero() -> Int { return Int(zero()) }
}

// CHECK: define internal i32 @innerZero() #2 {

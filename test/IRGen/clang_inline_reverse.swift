// Same test as clang_inline.swift, but with the order swapped.

// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -assume-parsing-unqualified-ownership-sil -sdk %S/Inputs -primary-file %s -emit-ir -module-name clang_inline | %FileCheck %s

// REQUIRES: CPU=i386 || CPU=x86_64
// XFAIL: linux

import gizmo

// CHECK-LABEL: define hidden swiftcc i64 @_T012clang_inline16CallStaticInlineC10ReturnZeros5Int64VyF(%C12clang_inline16CallStaticInline* swiftself) {{.*}} {
class CallStaticInline {
  func ReturnZero() -> Int64 { return Int64(wrappedZero()) }
}

// CHECK-LABEL: define internal i32 @wrappedZero() {{#[0-9]+}} {

// CHECK-LABEL: define hidden swiftcc i64 @_T012clang_inline17CallStaticInline2C10ReturnZeros5Int64VyF(%C12clang_inline17CallStaticInline2* swiftself) {{.*}} {
class CallStaticInline2 {
  func ReturnZero() -> Int64 { return Int64(zero()) }
}

// CHECK-LABEL: define internal i32 @zero() {{#[0-9]+}} {

// CHECK-LABEL: define internal i32 @innerZero() {{#[0-9]+}} {

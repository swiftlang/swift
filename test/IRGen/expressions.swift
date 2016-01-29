// RUN: %target-swift-frontend -primary-file %s -emit-ir -parse-stdlib -disable-access-control | FileCheck %s

// REQUIRES: CPU=x86_64

import Swift


// CHECK: @[[const1:[0-9]+]] = private unnamed_addr constant [22 x i8] c"this is just a\0A\0A test\00"
// CHECK: @[[const2:[0-9]+]] = private unnamed_addr constant [19 x i16] [i16 110, i16 111, i16 110, i16 45, i16 65, i16 83, i16 67, i16 73, i16 73, i16 32, i16 115, i16 116, i16 114, i16 105, i16 110, i16 103, i16 32, i16 181, i16 0]

// CHECK: define hidden [[stringLayout:[^@]*]] @_TF11expressions17TestStringLiteralFT_SS() {{.*}} {
// CHECK: call [[stringLayout]] @{{.*}}_builtinStringLiteral{{.*}}(i8* getelementptr inbounds ([22 x i8], [22 x i8]* @[[const1]], i64 0, i64 0), i64 21, i1 true)

func TestStringLiteral() -> String {
  return "this is just a\n\u{0a} test"
}

// CHECK: define hidden [[stringLayout]] @_TF11expressions18TestStringLiteral2FT_SS() {{.*}} {
// CHECK: call [[stringLayout]] @{{.*}}_builtinUTF16StringLiteral{{.*}}(i8* bitcast ([19 x i16]* @[[const2]] to i8*), i64 18)
func TestStringLiteral2() -> String {
  return "non-ASCII string \u{00B5}"
}

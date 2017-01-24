// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -assume-parsing-unqualified-ownership-sil -primary-file %s -emit-ir -parse-stdlib -disable-access-control | %FileCheck %s

// REQUIRES: CPU=x86_64

import Swift


// CHECK: private unnamed_addr constant [22 x i8] c"this is just a\0A\0A test\00"

// CHECK: define hidden [[stringLayout:[^@]*]] @_T011expressions17TestStringLiteralSSyF() {{.*}} {
// CHECK: call [[stringLayout]] @{{.*}}_builtinStringLiteral{{.*}}(i8* getelementptr inbounds ([22 x i8], [22 x i8]* @0, i64 0, i64 0), i64 21, i1 true)

func TestStringLiteral() -> String {
  return "this is just a\n\u{0a} test"
}

// CHECK: define hidden [[stringLayout]] @_T011expressions18TestStringLiteral2SSyF() {{.*}} {
// CHECK: call [[stringLayout]] @{{.*}}_builtinUTF16StringLiteral{{.*}}(i8* bitcast ([19 x i16]* @1 to i8*), i64 18)
func TestStringLiteral2() -> String {
  return "non-ASCII string \u{00B5}"
}

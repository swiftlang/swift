// RUN: %target-swift-frontend %use_no_opaque_pointers -primary-file %s -emit-ir -parse-stdlib -disable-access-control | %FileCheck %s
// RUN: %target-swift-frontend -primary-file %s -emit-ir -parse-stdlib -disable-access-control

// REQUIRES: PTRSIZE=64

import Swift


// CHECK: define hidden [[stringLayout:[^@]*]] @"$s11expressions17TestStringLiteralSSyF"() {{.*}} {
// CHECK: call [[stringLayout]] @{{.*}}_builtinStringLiteral{{.*}}(i8* getelementptr inbounds ([22 x i8], [22 x i8]* @".str.21.this is just a\0A\0A test", i64 0, i64 0), i64 21, i1 true)

func TestStringLiteral() -> String {
  return "this is just a\n\u{0a} test"
}

// CHECK: define hidden [[stringLayout]] @"$s11expressions18TestStringLiteral2SSyF"() {{.*}} {
// CHECK: call [[stringLayout]] @{{.*}}_builtinStringLiteral{{.*}}(i8* getelementptr inbounds ([20 x i8], [20 x i8]* @".str.19.non-ASCII string \C2\B5", i64 0, i64 0), i64 19, i1 false)
func TestStringLiteral2() -> String {
  return "non-ASCII string \u{00B5}"
}

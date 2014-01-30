// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -target x86_64-apple-darwin10 -module-cache-path=%t/clang-module-cache -sdk %S/Inputs %s -emit-ir | FileCheck %s
import gizmo

// CHECK: define i64 @_TFC12clang_inline16CallStaticInline10ReturnZerofS0_FT_Si(%C12clang_inline16CallStaticInline*) {
// CHECK: define internal i32 @zero() #0 {
// FIXME: We should be generating the IR for the body of innerZero as well.
// CHECK: declare i32 @innerZero() #1
class CallStaticInline {
  func ReturnZero() -> Int { return Int(zero()) }
}

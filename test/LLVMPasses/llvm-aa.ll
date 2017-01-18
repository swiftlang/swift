; RUN: %swift-llvm-opt -swift-aa -gvn %s | %FileCheck %s

target datalayout = "e-p:64:64:64-S128-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f16:16:16-f32:32:32-f64:64:64-f128:128:128-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64"
target triple = "x86_64-apple-macosx10.9"

declare void @swift_retain(i8 *) nounwind

; CHECK-LABEL: define{{( protected)?}} i8 @test_eliminate_loads_over_retain(i8*) {
; CHECK: load
; CHECK-NOT: load
define i8 @test_eliminate_loads_over_retain(i8*) {
entry:
  %1 = load i8, i8* %0
  tail call void @swift_retain(i8* %0)
  %2 = load i8, i8* %0
  %3 = add i8 %1, %2
  ret i8 %3
}

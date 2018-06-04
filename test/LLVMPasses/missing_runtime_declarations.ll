; RUN: %swift-llvm-opt -swift-arc-contract %s | %FileCheck %s

target datalayout = "e-p:64:64:64-S128-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f16:16:16-f32:32:32-f64:64:64-f128:128:128-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64"
target triple = "x86_64-apple-macosx10.9"

; CHECK: %a = type opaque
; CHECK: %swift.bridge = type opaque
; CHECK: declare void @swift_bridgeObjectRelease(%a* nocapture)

%a = type opaque
declare void @swift_bridgeObjectRelease(%a* nocapture)

; CHECK-LABEL: define void @testcase(%a*) {
; CHECK: entry:
; CHECK-NEXT: [[CAST:%.*]] = bitcast %a* %0 to %swift.bridge*
; CHECK-NEXT: call void @swift_bridgeObjectRelease_n(%swift.bridge* [[CAST]], i32 2)
; CHECK-NEXT: ret void
define void @testcase(%a*) {
entry:
  call void @swift_bridgeObjectRelease(%a* %0)
  call void @swift_bridgeObjectRelease(%a* %0)
  ret void
}
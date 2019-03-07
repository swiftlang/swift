; RUN: %swift-llvm-opt -swift-arc-contract %s | %FileCheck %s

target datalayout = "e-p:64:64:64-S128-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f16:16:16-f32:32:32-f64:64:64-f128:128:128-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64"
target triple = "x86_64-apple-macosx10.9"

; CHECK: %a = type opaque
; CHECK: %swift.bridge = type opaque
; CHECK: declare void @swift_bridgeObjectRelease(%a* nocapture)

%a = type opaque
declare void @swift_bridgeObjectRelease(%a* nocapture)
declare %a *@swift_bridgeObjectRetain(%a* nocapture)

; CHECK-LABEL: define void @testcase1(%a*) {
; CHECK: entry:
; CHECK-NEXT: [[CAST:%.*]] = bitcast %a* %0 to %swift.bridge*
; CHECK-NEXT: call void @swift_bridgeObjectRelease_n(%swift.bridge* [[CAST]], i32 2)
; CHECK-NEXT: ret void
define void @testcase1(%a*) {
entry:
  call void @swift_bridgeObjectRelease(%a* %0)
  call void @swift_bridgeObjectRelease(%a* %0)
  ret void
}

declare void @user(%a*)

; CHECK-LABEL: define %a* @testcase2(%a*) {
; CHECK: entry:
; CHECK-NEXT: [[CAST1:%.*]] = bitcast %a* %0 to %swift.bridge*
; CHECK-NEXT: [[RESULT:%.*]] ={{( tail)?}} call %swift.bridge* @swift_bridgeObjectRetain_n(%swift.bridge* [[CAST1]], i32 2)
; CHECK-NEXT: [[RESULT_CAST1:%.*]] = bitcast %swift.bridge* %2 to %a*
; CHECK-NEXT: [[RESULT_CAST2:%.*]] = bitcast %swift.bridge* %2 to %a*
; CHECK-NEXT: call void @user(%a* [[RESULT_CAST1]])
; CHECK-NEXT: call void @user(%a* [[RESULT_CAST2]])
; CHECK-NEXT: ret %a* [[RESULT_CAST2]]
define %a* @testcase2(%a*) {
entry:
  %1 = call %a* @swift_bridgeObjectRetain(%a* %0)
  %2 = call %a* @swift_bridgeObjectRetain(%a* %0)
  call void @user(%a* %1)
  call void @user(%a* %2)
  ret %a* %2
}
; RUN: %swift-llvm-opt -passes=swift-llvm-arc-contract %s | %FileCheck %s

target datalayout = "e-p:64:64:64-S128-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f16:16:16-f32:32:32-f64:64:64-f128:128:128-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64"
target triple = "x86_64-apple-macosx10.9"

; CHECK: declare void @swift_bridgeObjectRelease(ptr nocapture)

%a = type opaque
declare void @swift_bridgeObjectRelease(ptr nocapture)
declare ptr @swift_bridgeObjectRetain(ptr nocapture)

; CHECK-LABEL: define void @testcase1(ptr %0) {
; CHECK: entry:
; CHECK-NEXT: call void @swift_bridgeObjectRelease_n(ptr %0, i32 2)
; CHECK-NEXT: ret void
define void @testcase1(ptr) {
entry:
  call void @swift_bridgeObjectRelease(ptr %0)
  call void @swift_bridgeObjectRelease(ptr %0)
  ret void
}

declare void @user(ptr)

; CHECK-LABEL: define ptr @testcase2(ptr %0) {
; CHECK: entry:
; CHECK-NEXT: [[RESULT:%.*]] ={{( tail)?}} call ptr @swift_bridgeObjectRetain_n(ptr %0, i32 2)
; CHECK-NEXT: call void @user(ptr %1)
; CHECK-NEXT: call void @user(ptr %1)
; CHECK-NEXT: ret ptr %1
define ptr @testcase2(ptr) {
entry:
  %1 = call ptr @swift_bridgeObjectRetain(ptr %0)
  %2 = call ptr @swift_bridgeObjectRetain(ptr %0)
  call void @user(ptr %1)
  call void @user(ptr %2)
  ret ptr %2
}

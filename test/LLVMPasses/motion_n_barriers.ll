; RUN: %swift-llvm-opt -passes=swift-llvm-arc-optimize %s | %FileCheck %s

; Check that retain and release motion don't move a retain or release past a
; *_n retain or release.

target datalayout = "e-p:64:64:64-S128-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f16:16:16-f32:32:32-f64:64:64-f128:128:128-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64"
target triple = "x86_64-apple-macosx10.9"

declare ptr @swift_retain(ptr returned) nounwind
declare void @swift_release(ptr captures(none)) nounwind
declare ptr @swift_retain_n(ptr returned, i32) nounwind
declare void @swift_release_n(ptr captures(none), i32) nounwind
declare ptr @swift_unknownObjectRetain_n(ptr returned, i32) nounwind
declare void @swift_unknownObjectRelease_n(ptr, i32) nounwind
declare ptr @swift_bridgeObjectRetain_n(ptr, i32) nounwind
declare void @swift_bridgeObjectRelease_n(ptr, i32) nounwind

declare void @user(ptr) nounwind

; CHECK-LABEL: @release_after_retain_n(
; CHECK-NEXT: entry:
; CHECK-NEXT: call ptr @swift_retain_n(ptr %P, i32 1)
; CHECK-NEXT: call void @swift_release(ptr %P)
; CHECK-NEXT: ret void
define void @release_after_retain_n(ptr %P) {
entry:
  call ptr @swift_retain_n(ptr %P, i32 1)
  call void @swift_release(ptr %P)
  ret void
}

; CHECK-LABEL: @release_after_unknown_object_retain_n(
; CHECK-NEXT: entry:
; CHECK-NEXT: call ptr @swift_unknownObjectRetain_n(ptr %P, i32 1)
; CHECK-NEXT: call void @swift_release(ptr %P)
; CHECK-NEXT: ret void
define void @release_after_unknown_object_retain_n(ptr %P) {
entry:
  call ptr @swift_unknownObjectRetain_n(ptr %P, i32 1)
  call void @swift_release(ptr %P)
  ret void
}

; CHECK-LABEL: @release_after_bridge_retain_n(
; CHECK-NEXT: entry:
; CHECK-NEXT: call ptr @swift_bridgeObjectRetain_n(ptr %P, i32 1)
; CHECK-NEXT: call void @swift_release(ptr %P)
; CHECK-NEXT: ret void
define void @release_after_bridge_retain_n(ptr %P) {
entry:
  call ptr @swift_bridgeObjectRetain_n(ptr %P, i32 1)
  call void @swift_release(ptr %P)
  ret void
}

; CHECK-LABEL: @retain_before_release_n(
; CHECK-NEXT: entry:
; CHECK-NEXT: call ptr @swift_retain(ptr %P)
; CHECK-NEXT: call void @swift_release_n(ptr %P, i32 1)
; CHECK-NEXT: call void @user(ptr %P)
; CHECK-NEXT: ret void
define void @retain_before_release_n(ptr %P) {
entry:
  call ptr @swift_retain(ptr %P)
  call void @swift_release_n(ptr %P, i32 1)
  call void @user(ptr %P)
  ret void
}

; CHECK-LABEL: @retain_before_unknown_object_release_n(
; CHECK-NEXT: entry:
; CHECK-NEXT: call ptr @swift_retain(ptr %P)
; CHECK-NEXT: call void @swift_unknownObjectRelease_n(ptr %P, i32 1)
; CHECK-NEXT: call void @user(ptr %P)
; CHECK-NEXT: ret void
define void @retain_before_unknown_object_release_n(ptr %P) {
entry:
  call ptr @swift_retain(ptr %P)
  call void @swift_unknownObjectRelease_n(ptr %P, i32 1)
  call void @user(ptr %P)
  ret void
}

; CHECK-LABEL: @retain_before_bridge_release_n(
; CHECK-NEXT: entry:
; CHECK-NEXT: call ptr @swift_retain(ptr %P)
; CHECK-NEXT: call void @swift_bridgeObjectRelease_n(ptr %P, i32 1)
; CHECK-NEXT: call void @user(ptr %P)
; CHECK-NEXT: ret void
define void @retain_before_bridge_release_n(ptr %P) {
entry:
  call ptr @swift_retain(ptr %P)
  call void @swift_bridgeObjectRelease_n(ptr %P, i32 1)
  call void @user(ptr %P)
  ret void
}

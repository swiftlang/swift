; RUN: %swift-llvm-opt -passes=swift-llvm-arc-contract %s | %FileCheck %s

target datalayout = "e-p:64:64:64-S128-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f16:16:16-f32:32:32-f64:64:64-f128:128:128-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64"
target triple = "x86_64-apple-macosx10.9"

%swift.refcounted = type { ptr, i64 }
%swift.heapmetadata = type { ptr, ptr }
%swift.bridge = type opaque

declare ptr @swift_allocObject(ptr , i64, i64) nounwind
declare ptr @swift_bridgeObjectRetain(ptr)
declare void @swift_bridgeObjectRelease(ptr )
declare void @swift_release(ptr nocapture)
declare ptr @swift_retain(ptr ) nounwind
declare void @swift_unknownObjectRelease(ptr nocapture)
declare ptr @swift_unknownObjectRetain(ptr ) nounwind
declare void @__swift_fixLifetime(ptr)
declare void @noread_user(ptr) readnone
declare void @user(ptr)
declare void @noread_user_bridged(ptr) readnone
declare void @user_bridged(ptr)
declare void @__swift_endBorrow(ptr, ptr)

; CHECK-LABEL: define{{( protected)?}} void @fixlifetime_removal(ptr %0) {
; CHECK-NOT: call void @__swift_fixLifetime
define void @fixlifetime_removal(ptr) {
entry:
  call void @__swift_fixLifetime(ptr %0)
  ret void
}

; CHECK-LABEL: define{{( protected)?}} void @endBorrow_removal(ptr %0, ptr %1) {
; CHECK-NOT: call void @__swift_endBorrow
define void @endBorrow_removal(ptr, ptr) {
entry:
  call void @__swift_endBorrow(ptr %0, ptr %1)
  ret void
}

; CHECK-LABEL: define{{( protected)?}} ptr @swift_contractRetainN(ptr %A) {
; CHECK: entry:
; CHECK-NEXT: br i1 undef
; CHECK: bb1:
; CHECK-NEXT: call ptr @swift_retain_n(ptr %A, i32 2)
; CHECK-NEXT: call void @noread_user(ptr %A)
; CHECK-NEXT: call void @noread_user(ptr %A)
; CHECK-NEXT: br label %bb3
; CHECK: bb2:
; CHECK-NEXT: call void @noread_user(ptr %A)
; CHECK-NEXT: call ptr @swift_retain(ptr %A)
; CHECK-NEXT: call void @noread_user(ptr %A)
; CHECK-NEXT: br label %bb3
; CHECK: bb3:
; CHECK-NEXT: call ptr @swift_retain(ptr %A)
; CHECK-NEXT: ret ptr %A
define ptr @swift_contractRetainN(ptr %A) {
entry:
  br i1 undef, label %bb1, label %bb2

bb1:
  call ptr @swift_retain(ptr %A)
  call void @noread_user(ptr %A)
  call ptr @swift_retain(ptr %A)
  call void @noread_user(ptr %A)
  br label %bb3

bb2:
  call void @noread_user(ptr %A)
  call ptr @swift_retain(ptr %A)
  call void @noread_user(ptr %A)
  br label %bb3

bb3:
  call ptr @swift_retain(ptr %A)
  ret ptr %A
}

; CHECK-LABEL: define{{( protected)?}} ptr @swift_contractRetainNWithRCIdentity(ptr %A) {
; CHECK: entry:
; CHECK-NEXT: br i1 undef
; CHECK: bb1:
; CHECK-NEXT: call ptr @swift_retain_n(ptr %A, i32 2)
; CHECK-NEXT: br label %bb3
; CHECK: bb2:
; CHECK-NEXT: call ptr @swift_retain(ptr %A)
; CHECK-NEXT: br label %bb3
; CHECK: bb3:
; CHECK-NEXT: call ptr @swift_retain(ptr %A)
; CHECK-NEXT: ret ptr %A
define ptr @swift_contractRetainNWithRCIdentity(ptr %A) {
entry:
  br i1 undef, label %bb1, label %bb2

bb1:
  call ptr @swift_retain(ptr %A)
  call ptr @swift_retain(ptr %A)
  br label %bb3

bb2:
  call ptr @swift_retain(ptr %A)
  br label %bb3

bb3:
  call ptr @swift_retain(ptr %A)
  ret ptr %A
}

; CHECK-LABEL: define{{( protected)?}} ptr @swift_contractReleaseN(ptr %A) {
; CHECK: entry:
; CHECK-NEXT: br i1 undef
; CHECK: bb1:
; CHECK-NEXT: call void @noread_user(ptr %A)
; CHECK-NEXT: call void @swift_release_n(ptr %A, i32 2)
; CHECK-NEXT: call void @noread_user(ptr %A)
; CHECK-NEXT: br label %bb3
; CHECK: bb2:
; CHECK-NEXT: call void @noread_user(ptr %A)
; CHECK-NEXT: call void @swift_release(ptr %A)
; CHECK-NEXT: call void @noread_user(ptr %A)
; CHECK-NEXT: br label %bb3
; CHECK: bb3:
; CHECK-NEXT: call void @swift_release(ptr %A)
; CHECK-NEXT: ret ptr %A
define ptr @swift_contractReleaseN(ptr %A) {
entry:
  br i1 undef, label %bb1, label %bb2

bb1:
  call void @swift_release(ptr %A)
  call void @noread_user(ptr %A)
  call void @swift_release(ptr %A)
  call void @noread_user(ptr %A)
  br label %bb3

bb2:
  call void @noread_user(ptr %A)
  call void @swift_release(ptr %A)
  call void @noread_user(ptr %A)
  br label %bb3

bb3:
  call void @swift_release(ptr %A)
  ret ptr %A
}

; CHECK-LABEL: define{{( protected)?}} ptr @swift_contractReleaseNWithRCIdentity(ptr %A) {
; CHECK: entry:
; CHECK-NEXT: br i1 undef
; CHECK: bb1:
; CHECK-NEXT: call void @swift_release_n(ptr %A, i32 2)
; CHECK-NEXT: br label %bb3
; CHECK: bb2:
; CHECK-NEXT: call void @swift_release(ptr %A)
; CHECK-NEXT: br label %bb3
; CHECK: bb3:
; CHECK-NEXT: call void @swift_release(ptr %A)
; CHECK-NEXT: ret ptr %A
define ptr @swift_contractReleaseNWithRCIdentity(ptr %A) {
entry:
  br i1 undef, label %bb1, label %bb2

bb1:
  call void @swift_release(ptr %A)
  call void @swift_release(ptr %A)
  br label %bb3

bb2:
  call void @swift_release(ptr %A)
  br label %bb3

bb3:
  call void @swift_release(ptr %A)
  ret ptr %A
}


; Make sure that we do not form retainN,releaseN over uses that may
; read the reference count of the object.

; CHECK-LABEL: define{{( protected)?}} ptr @swift_contractRetainNWithUnknown(ptr %A) {
; CHECK-NOT: call ptr @swift_retain_n
define ptr @swift_contractRetainNWithUnknown(ptr %A) {
entry:
  br i1 undef, label %bb1, label %bb2

bb1:
  call ptr @swift_retain(ptr %A)
  call void @user(ptr %A)
  call ptr @swift_retain(ptr %A)
  call void @user(ptr %A)
  br label %bb3

bb2:
  call void @user(ptr %A)
  call ptr @swift_retain(ptr %A)
  call void @user(ptr %A)
  br label %bb3

bb3:
  call ptr @swift_retain(ptr %A)
  ret ptr %A
}

; CHECK-LABEL: define{{( protected)?}} ptr @swift_contractReleaseNWithUnknown(ptr %A) {
; CHECK-NOT: call void @swift_release_n
define ptr @swift_contractReleaseNWithUnknown(ptr %A) {
entry:
  br i1 undef, label %bb1, label %bb2

bb1:
  call void @swift_release(ptr %A)
  call void @user(ptr %A)
  call void @swift_release(ptr %A)
  call void @user(ptr %A)
  br label %bb3

bb2:
  call void @user(ptr %A)
  call void @swift_release(ptr %A)
  call void @user(ptr %A)
  br label %bb3

bb3:
  call void @swift_release(ptr %A)
  ret ptr %A
}

; But do make sure that we can form retainN, releaseN in between such uses
; CHECK-LABEL: define{{( protected)?}} ptr @swift_contractRetainNInterleavedWithUnknown(ptr %A) {
; CHECK: bb1:
; CHECK: call ptr @swift_retain(ptr %A)
; CHECK-NEXT: call void @user(ptr %A)
; CHECK-NEXT: call ptr @swift_retain_n(ptr %A, i32 3)
; CHECK-NEXT: call void @noread_user(ptr %A)
; CHECK-NEXT: call void @noread_user(ptr %A)
; CHECK-NEXT: call void @user(ptr %A)
; CHECK-NEXT: call ptr @swift_retain_n(ptr %A, i32 2)
; CHECK-NEXT: call void @user(ptr %A)
; CHECK-NEXT: call ptr @swift_retain(ptr %A)
; CHECK-NEXT: br label %bb3

; CHECK: bb2:
; CHECK-NEXT: call void @user(ptr %A)
; CHECK-NEXT: call ptr @swift_retain(ptr %A)
; CHECK-NEXT: call void @user(ptr %A)
; CHECK-NEXT: br label %bb3

; CHECK: bb3:
; CHECK-NEXT: call ptr @swift_retain(ptr %A)
; CHECK-NEXT: ret ptr %A
define ptr @swift_contractRetainNInterleavedWithUnknown(ptr %A) {
entry:
  br i1 undef, label %bb1, label %bb2

bb1:
  call ptr @swift_retain(ptr %A)
  call void @user(ptr %A)
  call ptr @swift_retain(ptr %A)
  call void @noread_user(ptr %A)
  call ptr @swift_retain(ptr %A)
  call void @noread_user(ptr %A)
  call ptr @swift_retain(ptr %A)
  call void @user(ptr %A)
  call ptr @swift_retain(ptr %A)
  call ptr @swift_retain(ptr %A)
  call void @user(ptr %A)
  call ptr @swift_retain(ptr %A)
  br label %bb3

bb2:
  call void @user(ptr %A)
  call ptr @swift_retain(ptr %A)
  call void @user(ptr %A)
  br label %bb3

bb3:
  call ptr @swift_retain(ptr %A)
  ret ptr %A
}

; CHECK-LABEL: define{{( protected)?}} ptr @swift_contractReleaseNInterleavedWithUnknown(ptr %A) {
; CHECK: bb1:
; CHECK-NEXT: @swift_release(
; CHECK-NEXT: @user
; CHECK-NEXT: @noread_user
; CHECK-NEXT: @swift_release_n
; CHECK-NEXT: @user
; CHECK-NEXT: br label %bb3
define ptr @swift_contractReleaseNInterleavedWithUnknown(ptr %A) {
entry:
  br i1 undef, label %bb1, label %bb2

bb1:
  call void @swift_release(ptr %A)
  call void @user(ptr %A)
  call void @swift_release(ptr %A)
  call void @noread_user(ptr %A)
  call void @swift_release(ptr %A)
  call void @user(ptr %A)
  br label %bb3

bb2:
  call void @user(ptr %A)
  call void @swift_release(ptr %A)
  call void @user(ptr %A)
  br label %bb3

bb3:
  call void @swift_release(ptr %A)
  ret ptr %A
}

; CHECK-LABEL: define{{( protected)?}} ptr @swift_contractRetainReleaseNInterleavedWithUnknown(ptr %A) {
; CHECK: bb1:
; CHECK-NEXT: call ptr @swift_retain(ptr %A)
; CHECK-NEXT: call void @user(ptr %A)
; CHECK-NEXT: call void @noread_user(ptr %A)
; CHECK-NEXT: call void @swift_release_n(ptr %A, i32 2)
; CHECK-NEXT: call void @user(ptr %A)
; CHECK-NEXT: call ptr @swift_retain_n(ptr %A, i32 2)
; CHECK-NEXT: call void @swift_release(ptr %A)
; CHECK-NEXT: call void @user(ptr %A)
; CHECK-NEXT: call void @noread_user(ptr %A)
; CHECK-NEXT: call void @swift_release_n(ptr %A, i32 2)
; CHECK-NEXT: call void @user(ptr %A)
; CHECK-NEXT: br label %bb3
; CHECK: bb2:
; CHECK-NEXT: call void @user(ptr %A)
; CHECK-NEXT: call void @swift_release(ptr %A)
; CHECK-NEXT: call void @user(ptr %A)
; CHECK-NEXT: br label %bb3

; CHECK: bb3:
; CHECK-NEXT: call void @swift_release(ptr %A)
; CHECK-NEXT: ret ptr %A
define ptr @swift_contractRetainReleaseNInterleavedWithUnknown(ptr %A) {
entry:
  br i1 undef, label %bb1, label %bb2

bb1:
  call ptr @swift_retain(ptr %A)
  call void @user(ptr %A)
  call void @swift_release(ptr %A)
  call void @noread_user(ptr %A)
  call void @swift_release(ptr %A)
  call void @user(ptr %A)
  call ptr @swift_retain(ptr %A)
  call ptr @swift_retain(ptr %A)
  call void @swift_release(ptr %A)
  call void @user(ptr %A)
  call void @swift_release(ptr %A)
  call void @noread_user(ptr %A)
  call void @swift_release(ptr %A)
  call void @user(ptr %A)

  br label %bb3

bb2:
  call void @user(ptr %A)
  call void @swift_release(ptr %A)
  call void @user(ptr %A)
  br label %bb3

bb3:
  call void @swift_release(ptr %A)
  ret ptr %A
}

; CHECK-LABEL: define{{( protected)?}} ptr @swift_contractUnknownObjectRetainNInterleavedWithUnknown(ptr %A) {
; CHECK: bb1:
; CHECK: call ptr @swift_unknownObjectRetain(ptr %A)
; CHECK-NEXT: call void @user(ptr %A)
; CHECK-NEXT: call ptr @swift_unknownObjectRetain_n(ptr %A, i32 3)
; CHECK-NEXT: call void @noread_user(ptr %A)
; CHECK-NEXT: call void @noread_user(ptr %A)
; CHECK-NEXT: call void @user(ptr %A)
; CHECK-NEXT: call ptr @swift_unknownObjectRetain_n(ptr %A, i32 2)
; CHECK-NEXT: call void @user(ptr %A)
; CHECK-NEXT: call ptr @swift_unknownObjectRetain(ptr %A)
; CHECK-NEXT: br label %bb3

; CHECK: bb2:
; CHECK-NEXT: call void @user(ptr %A)
; CHECK-NEXT: call ptr @swift_unknownObjectRetain(ptr %A)
; CHECK-NEXT: call void @user(ptr %A)
; CHECK-NEXT: br label %bb3

; CHECK: bb3:
; CHECK-NEXT: call ptr @swift_unknownObjectRetain(ptr %A)
; CHECK-NEXT: ret ptr %A
define ptr @swift_contractUnknownObjectRetainNInterleavedWithUnknown(ptr %A) {
entry:
  br i1 undef, label %bb1, label %bb2

bb1:
  call ptr @swift_unknownObjectRetain(ptr %A)
  call void @user(ptr %A)
  call ptr @swift_unknownObjectRetain(ptr %A)
  call void @noread_user(ptr %A)
  call ptr @swift_unknownObjectRetain(ptr %A)
  call void @noread_user(ptr %A)
  call ptr @swift_unknownObjectRetain(ptr %A)
  call void @user(ptr %A)
  call ptr @swift_unknownObjectRetain(ptr %A)
  call ptr @swift_unknownObjectRetain(ptr %A)
  call void @user(ptr %A)
  call ptr @swift_unknownObjectRetain(ptr %A)
  br label %bb3

bb2:
  call void @user(ptr %A)
  call ptr @swift_unknownObjectRetain(ptr %A)
  call void @user(ptr %A)
  br label %bb3

bb3:
  call ptr @swift_unknownObjectRetain(ptr %A)
  ret ptr %A
}

; CHECK-LABEL: define{{( protected)?}} ptr @swift_contractUnknownObjectReleaseNInterleavedWithUnknown(ptr %A) {
; CHECK: bb1:
; CHECK-NEXT: @swift_unknownObjectRelease(
; CHECK-NEXT: @user
; CHECK-NEXT: @noread_user
; CHECK-NEXT: @swift_unknownObjectRelease_n
; CHECK-NEXT: @user
; CHECK-NEXT: br label %bb3
define ptr @swift_contractUnknownObjectReleaseNInterleavedWithUnknown(ptr %A) {
entry:
  br i1 undef, label %bb1, label %bb2

bb1:
  call void @swift_unknownObjectRelease(ptr %A)
  call void @user(ptr %A)
  call void @swift_unknownObjectRelease(ptr %A)
  call void @noread_user(ptr %A)
  call void @swift_unknownObjectRelease(ptr %A)
  call void @user(ptr %A)
  br label %bb3

bb2:
  call void @user(ptr %A)
  call void @swift_unknownObjectRelease(ptr %A)
  call void @user(ptr %A)
  br label %bb3

bb3:
  call void @swift_unknownObjectRelease(ptr %A)
  ret ptr %A
}

; CHECK-LABEL: define{{( protected)?}} ptr @swift_contractUnknownObjectRetainReleaseNInterleavedWithUnknown(ptr %A) {
; CHECK: bb1:
; CHECK-NEXT: call ptr @swift_unknownObjectRetain(ptr %A)
; CHECK-NEXT: call void @user(ptr %A)
; CHECK-NEXT: call void @noread_user(ptr %A)
; CHECK-NEXT: call void @swift_unknownObjectRelease_n(ptr %A, i32 2)
; CHECK-NEXT: call void @user(ptr %A)
; CHECK-NEXT: call ptr @swift_unknownObjectRetain_n(ptr %A, i32 2)
; CHECK-NEXT: call void @swift_unknownObjectRelease(ptr %A)
; CHECK-NEXT: call void @user(ptr %A)
; CHECK-NEXT: call void @noread_user(ptr %A)
; CHECK-NEXT: call void @swift_unknownObjectRelease_n(ptr %A, i32 2)
; CHECK-NEXT: call void @user(ptr %A)
; CHECK-NEXT: br label %bb3
; CHECK: bb2:
; CHECK-NEXT: call void @user(ptr %A)
; CHECK-NEXT: call void @swift_unknownObjectRelease(ptr %A)
; CHECK-NEXT: call void @user(ptr %A)
; CHECK-NEXT: br label %bb3

; CHECK: bb3:
; CHECK-NEXT: call void @swift_unknownObjectRelease(ptr %A)
; CHECK-NEXT: ret ptr %A
define ptr @swift_contractUnknownObjectRetainReleaseNInterleavedWithUnknown(ptr %A) {
entry:
  br i1 undef, label %bb1, label %bb2

bb1:
  call ptr @swift_unknownObjectRetain(ptr %A)
  call void @user(ptr %A)
  call void @swift_unknownObjectRelease(ptr %A)
  call void @noread_user(ptr %A)
  call void @swift_unknownObjectRelease(ptr %A)
  call void @user(ptr %A)
  call ptr @swift_unknownObjectRetain(ptr %A)
  call ptr @swift_unknownObjectRetain(ptr %A)
  call void @swift_unknownObjectRelease(ptr %A)
  call void @user(ptr %A)
  call void @swift_unknownObjectRelease(ptr %A)
  call void @noread_user(ptr %A)
  call void @swift_unknownObjectRelease(ptr %A)
  call void @user(ptr %A)
  br label %bb3

bb2:
  call void @user(ptr %A)
  call void @swift_unknownObjectRelease(ptr %A)
  call void @user(ptr %A)
  br label %bb3

bb3:
  call void @swift_unknownObjectRelease(ptr %A)
  ret ptr %A
}


; CHECK-LABEL: define{{( protected)?}} ptr @swift_contractBridgeRetainWithBridge(ptr %A) {
; CHECK: bb1:
; CHECK-NEXT: [[RET0:%.+]] = call ptr @swift_bridgeObjectRetain_n(ptr %A, i32 2)
; CHECK-NEXT: call void @swift_bridgeObjectRelease(ptr [[RET0:%.+]])
; CHECK-NEXT: call void @swift_bridgeObjectRelease(ptr %A)
; CHECK-NEXT: ret ptr %A
define ptr @swift_contractBridgeRetainWithBridge(ptr %A) {
bb1:
  %0 = call ptr @swift_bridgeObjectRetain(ptr %A)
  %1 = call ptr @swift_bridgeObjectRetain(ptr %A)
  call void @swift_bridgeObjectRelease(ptr %1)
  call void @swift_bridgeObjectRelease(ptr %A)
  ret ptr %A
}

; CHECK-LABEL: define{{( protected)?}} ptr @swift_contractBridgeRetainReleaseNInterleavedWithBridge(ptr %A) {
; CHECK: bb1:
; CHECK-NEXT: [[RET0:%.+]] = call ptr @swift_bridgeObjectRetain(ptr %A)
; CHECK-NEXT: call void @user_bridged(ptr %A)
; CHECK-NEXT: call void @noread_user_bridged(ptr %A)
; CHECK-NEXT: call void @swift_bridgeObjectRelease_n(ptr %A, i32 2)
; CHECK-NEXT: call void @user_bridged(ptr %A)
; CHECK-NEXT: [[RET1:%.+]] = call ptr @swift_bridgeObjectRetain_n(ptr %A, i32 2)
; CHECK-NEXT: call void @swift_bridgeObjectRelease(ptr %A)
; CHECK-NEXT: call void @user_bridged(ptr %A)
; CHECK-NEXT: call void @noread_user_bridged(ptr %A)
; CHECK-NEXT: call void @swift_bridgeObjectRelease_n(ptr %A, i32 2)
; CHECK-NEXT: call void @user_bridged(ptr %A)
; CHECK-NEXT: br label %bb3
; CHECK: bb2:
; CHECK-NEXT: call void @user_bridged(ptr %A)
; CHECK-NEXT: call void @swift_bridgeObjectRelease(ptr %A)
; CHECK-NEXT: call void @user_bridged(ptr %A)
; CHECK-NEXT: br label %bb3

; CHECK: bb3:
; CHECK-NEXT: call void @swift_bridgeObjectRelease(ptr %A)
; CHECK-NEXT: ret ptr %A
define ptr @swift_contractBridgeRetainReleaseNInterleavedWithBridge(ptr %A) {
entry:
  br i1 undef, label %bb1, label %bb2

bb1:
  call ptr @swift_bridgeObjectRetain(ptr %A)
  call void @user_bridged(ptr %A)
  call void @swift_bridgeObjectRelease(ptr %A)
  call void @noread_user_bridged(ptr %A)
  call void @swift_bridgeObjectRelease(ptr %A)
  call void @user_bridged(ptr %A)
  call ptr @swift_bridgeObjectRetain(ptr %A)
  call ptr @swift_bridgeObjectRetain(ptr %A)
  call void @swift_bridgeObjectRelease(ptr %A)
  call void @user_bridged(ptr %A)
  call void @swift_bridgeObjectRelease(ptr %A)
  call void @noread_user_bridged(ptr %A)
  call void @swift_bridgeObjectRelease(ptr %A)
  call void @user_bridged(ptr %A)
  br label %bb3

bb2:
  call void @user_bridged(ptr %A)
  call void @swift_bridgeObjectRelease(ptr %A)
  call void @user_bridged(ptr %A)
  br label %bb3

bb3:
  call void @swift_bridgeObjectRelease(ptr %A)
  ret ptr %A
}

!llvm.dbg.cu = !{!1}
!llvm.module.flags = !{!4}

!0 = !DILocation(line: 0, scope: !3)
!1 = distinct !DICompileUnit(language: DW_LANG_Swift, file: !2)
!2 = !DIFile(filename: "contract.swift", directory: "")
!3 = distinct !DISubprogram(name: "_", scope: !1, file: !2, type: !DISubroutineType(types: !{}))
!4 = !{i32 1, !"Debug Info Version", i32 3}

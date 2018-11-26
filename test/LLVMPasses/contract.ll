; RUN: %swift-llvm-opt -swift-arc-contract %s | %FileCheck %s

target datalayout = "e-p:64:64:64-S128-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f16:16:16-f32:32:32-f64:64:64-f128:128:128-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64"
target triple = "x86_64-apple-macosx10.9"

%swift.refcounted = type { %swift.heapmetadata*, i64 }
%swift.heapmetadata = type { i64 (%swift.refcounted*)*, i64 (%swift.refcounted*)* }
%swift.bridge = type opaque

declare %swift.refcounted* @swift_allocObject(%swift.heapmetadata* , i64, i64) nounwind
declare %swift.bridge* @swift_bridgeObjectRetain(%swift.bridge*)
declare void @swift_bridgeObjectRelease(%swift.bridge* )
declare void @swift_release(%swift.refcounted* nocapture)
declare %swift.refcounted* @swift_retain(%swift.refcounted* ) nounwind
declare void @swift_unknownObjectRelease(%swift.refcounted* nocapture)
declare %swift.refcounted* @swift_unknownObjectRetain(%swift.refcounted* ) nounwind
declare void @__swift_fixLifetime(%swift.refcounted*)
declare void @noread_user(%swift.refcounted*) readnone
declare void @user(%swift.refcounted*)
declare void @noread_user_bridged(%swift.bridge*) readnone
declare void @user_bridged(%swift.bridge*)
declare void @__swift_endBorrow(i8*, i8*)

; CHECK-LABEL: define{{( protected)?}} void @fixlifetime_removal(i8*) {
; CHECK-NOT: call void @__swift_fixLifetime
define void @fixlifetime_removal(i8*) {
entry:
  %1 = bitcast i8* %0 to %swift.refcounted*
  call void @__swift_fixLifetime(%swift.refcounted* %1)
  ret void
}

; CHECK-LABEL: define{{( protected)?}} void @endBorrow_removal(i8*, i8*) {
; CHECK-NOT: call void @__swift_endBorrow
define void @endBorrow_removal(i8*, i8*) {
entry:
  call void @__swift_endBorrow(i8* %0, i8* %1)
  ret void
}

; CHECK-LABEL: define{{( protected)?}} %swift.refcounted* @swift_contractRetainN(%swift.refcounted* %A) {
; CHECK: entry:
; CHECK-NEXT: br i1 undef
; CHECK: bb1:
; CHECK-NEXT: tail call %swift.refcounted* @swift_retain_n(%swift.refcounted* %A, i32 2)
; CHECK-NEXT: call void @noread_user(%swift.refcounted* %A)
; CHECK-NEXT: call void @noread_user(%swift.refcounted* %A)
; CHECK-NEXT: br label %bb3
; CHECK: bb2:
; CHECK-NEXT: call void @noread_user(%swift.refcounted* %A)
; CHECK-NEXT: tail call %swift.refcounted* @swift_retain(%swift.refcounted* %A)
; CHECK-NEXT: call void @noread_user(%swift.refcounted* %A)
; CHECK-NEXT: br label %bb3
; CHECK: bb3:
; CHECK-NEXT: tail call %swift.refcounted* @swift_retain(%swift.refcounted* %A)
; CHECK-NEXT: ret %swift.refcounted* %A
define %swift.refcounted* @swift_contractRetainN(%swift.refcounted* %A) {
entry:
  br i1 undef, label %bb1, label %bb2

bb1:
  tail call %swift.refcounted* @swift_retain(%swift.refcounted* %A)
  call void @noread_user(%swift.refcounted* %A)
  tail call %swift.refcounted* @swift_retain(%swift.refcounted* %A)
  call void @noread_user(%swift.refcounted* %A)
  br label %bb3

bb2:
  call void @noread_user(%swift.refcounted* %A)
  tail call %swift.refcounted* @swift_retain(%swift.refcounted* %A)
  call void @noread_user(%swift.refcounted* %A)
  br label %bb3

bb3:
  tail call %swift.refcounted* @swift_retain(%swift.refcounted* %A)
  ret %swift.refcounted* %A
}

; CHECK-LABEL: define{{( protected)?}} %swift.refcounted* @swift_contractRetainNWithRCIdentity(%swift.refcounted* %A) {
; CHECK: entry:
; CHECK-NEXT: br i1 undef
; CHECK: bb1:
; CHECK-NEXT: tail call %swift.refcounted* @swift_retain_n(%swift.refcounted* %A, i32 2)
; CHECK-NEXT: %1 = bitcast %swift.refcounted* %A to %swift.refcounted*
; CHECK-NEXT: br label %bb3
; CHECK: bb2:
; CHECK-NEXT: tail call %swift.refcounted* @swift_retain(%swift.refcounted* %A)
; CHECK-NEXT: br label %bb3
; CHECK: bb3:
; CHECK-NEXT: tail call %swift.refcounted* @swift_retain(%swift.refcounted* %A)
; CHECK-NEXT: ret %swift.refcounted* %A
define %swift.refcounted* @swift_contractRetainNWithRCIdentity(%swift.refcounted* %A) {
entry:
  br i1 undef, label %bb1, label %bb2

bb1:
  tail call %swift.refcounted* @swift_retain(%swift.refcounted* %A)
  %1 = bitcast %swift.refcounted* %A to %swift.refcounted*
  tail call %swift.refcounted* @swift_retain(%swift.refcounted* %1)
  br label %bb3

bb2:
  tail call %swift.refcounted* @swift_retain(%swift.refcounted* %A)
  br label %bb3

bb3:
  tail call %swift.refcounted* @swift_retain(%swift.refcounted* %A)
  ret %swift.refcounted* %A
}

; CHECK-LABEL: define{{( protected)?}} %swift.refcounted* @swift_contractReleaseN(%swift.refcounted* %A) {
; CHECK: entry:
; CHECK-NEXT: br i1 undef
; CHECK: bb1:
; CHECK-NEXT: call void @noread_user(%swift.refcounted* %A)
; CHECK-NEXT: tail call void @swift_release_n(%swift.refcounted* %A, i32 2)
; CHECK-NEXT: call void @noread_user(%swift.refcounted* %A)
; CHECK-NEXT: br label %bb3
; CHECK: bb2:
; CHECK-NEXT: call void @noread_user(%swift.refcounted* %A)
; CHECK-NEXT: tail call void @swift_release(%swift.refcounted* %A)
; CHECK-NEXT: call void @noread_user(%swift.refcounted* %A)
; CHECK-NEXT: br label %bb3
; CHECK: bb3:
; CHECK-NEXT: tail call void @swift_release(%swift.refcounted* %A)
; CHECK-NEXT: ret %swift.refcounted* %A
define %swift.refcounted* @swift_contractReleaseN(%swift.refcounted* %A) {
entry:
  br i1 undef, label %bb1, label %bb2

bb1:
  tail call void @swift_release(%swift.refcounted* %A)
  call void @noread_user(%swift.refcounted* %A)
  tail call void @swift_release(%swift.refcounted* %A)
  call void @noread_user(%swift.refcounted* %A)
  br label %bb3

bb2:
  call void @noread_user(%swift.refcounted* %A)
  tail call void @swift_release(%swift.refcounted* %A)
  call void @noread_user(%swift.refcounted* %A)
  br label %bb3

bb3:
  tail call void @swift_release(%swift.refcounted* %A)
  ret %swift.refcounted* %A
}

; CHECK-LABEL: define{{( protected)?}} %swift.refcounted* @swift_contractReleaseNWithRCIdentity(%swift.refcounted* %A) {
; CHECK: entry:
; CHECK-NEXT: br i1 undef
; CHECK: bb1:
; CHECK-NEXT: %0 = bitcast %swift.refcounted* %A to %swift.refcounted*
; CHECK-NEXT: tail call void @swift_release_n(%swift.refcounted* %A, i32 2)
; CHECK-NEXT: br label %bb3
; CHECK: bb2:
; CHECK-NEXT: tail call void @swift_release(%swift.refcounted* %A)
; CHECK-NEXT: br label %bb3
; CHECK: bb3:
; CHECK-NEXT: tail call void @swift_release(%swift.refcounted* %A)
; CHECK-NEXT: ret %swift.refcounted* %A
define %swift.refcounted* @swift_contractReleaseNWithRCIdentity(%swift.refcounted* %A) {
entry:
  br i1 undef, label %bb1, label %bb2

bb1:
  tail call void @swift_release(%swift.refcounted* %A)
  %0 = bitcast %swift.refcounted* %A to %swift.refcounted*
  tail call void @swift_release(%swift.refcounted* %0)
  br label %bb3

bb2:
  tail call void @swift_release(%swift.refcounted* %A)
  br label %bb3

bb3:
  tail call void @swift_release(%swift.refcounted* %A)
  ret %swift.refcounted* %A
}


; Make sure that we do not form retainN,releaseN over uses that may
; read the reference count of the object.

; CHECK-LABEL: define{{( protected)?}} %swift.refcounted* @swift_contractRetainNWithUnknown(%swift.refcounted* %A) {
; CHECK-NOT: call %swift.refcounted* @swift_retain_n
define %swift.refcounted* @swift_contractRetainNWithUnknown(%swift.refcounted* %A) {
entry:
  br i1 undef, label %bb1, label %bb2

bb1:
  tail call %swift.refcounted* @swift_retain(%swift.refcounted* %A)
  call void @user(%swift.refcounted* %A)
  tail call %swift.refcounted* @swift_retain(%swift.refcounted* %A)
  call void @user(%swift.refcounted* %A)
  br label %bb3

bb2:
  call void @user(%swift.refcounted* %A)
  tail call %swift.refcounted* @swift_retain(%swift.refcounted* %A)
  call void @user(%swift.refcounted* %A)
  br label %bb3

bb3:
  tail call %swift.refcounted* @swift_retain(%swift.refcounted* %A)
  ret %swift.refcounted* %A
}

; CHECK-LABEL: define{{( protected)?}} %swift.refcounted* @swift_contractReleaseNWithUnknown(%swift.refcounted* %A) {
; CHECK-NOT: call void @swift_release_n
define %swift.refcounted* @swift_contractReleaseNWithUnknown(%swift.refcounted* %A) {
entry:
  br i1 undef, label %bb1, label %bb2

bb1:
  tail call void @swift_release(%swift.refcounted* %A)
  call void @user(%swift.refcounted* %A)
  tail call void @swift_release(%swift.refcounted* %A)
  call void @user(%swift.refcounted* %A)
  br label %bb3

bb2:
  call void @user(%swift.refcounted* %A)
  tail call void @swift_release(%swift.refcounted* %A)
  call void @user(%swift.refcounted* %A)
  br label %bb3

bb3:
  tail call void @swift_release(%swift.refcounted* %A)
  ret %swift.refcounted* %A
}

; But do make sure that we can form retainN, releaseN in between such uses
; CHECK-LABEL: define{{( protected)?}} %swift.refcounted* @swift_contractRetainNInterleavedWithUnknown(%swift.refcounted* %A) {
; CHECK: bb1:
; CHECK: tail call %swift.refcounted* @swift_retain(%swift.refcounted* %A)
; CHECK-NEXT: call void @user(%swift.refcounted* %A)
; CHECK-NEXT: tail call %swift.refcounted* @swift_retain_n(%swift.refcounted* %A, i32 3)
; CHECK-NEXT: call void @noread_user(%swift.refcounted* %A)
; CHECK-NEXT: call void @noread_user(%swift.refcounted* %A)
; CHECK-NEXT: call void @user(%swift.refcounted* %A)
; CHECK-NEXT: tail call %swift.refcounted* @swift_retain_n(%swift.refcounted* %A, i32 2)
; CHECK-NEXT: call void @user(%swift.refcounted* %A)
; CHECK-NEXT: tail call %swift.refcounted* @swift_retain(%swift.refcounted* %A)
; CHECK-NEXT: br label %bb3

; CHECK: bb2:
; CHECK-NEXT: call void @user(%swift.refcounted* %A)
; CHECK-NEXT: tail call %swift.refcounted* @swift_retain(%swift.refcounted* %A)
; CHECK-NEXT: call void @user(%swift.refcounted* %A)
; CHECK-NEXT: br label %bb3

; CHECK: bb3:
; CHECK-NEXT: tail call %swift.refcounted* @swift_retain(%swift.refcounted* %A)
; CHECK-NEXT: ret %swift.refcounted* %A
define %swift.refcounted* @swift_contractRetainNInterleavedWithUnknown(%swift.refcounted* %A) {
entry:
  br i1 undef, label %bb1, label %bb2

bb1:
  tail call %swift.refcounted* @swift_retain(%swift.refcounted* %A)
  call void @user(%swift.refcounted* %A)
  tail call %swift.refcounted* @swift_retain(%swift.refcounted* %A)
  call void @noread_user(%swift.refcounted* %A)
  tail call %swift.refcounted* @swift_retain(%swift.refcounted* %A)
  call void @noread_user(%swift.refcounted* %A)
  tail call %swift.refcounted* @swift_retain(%swift.refcounted* %A)
  call void @user(%swift.refcounted* %A)
  tail call %swift.refcounted* @swift_retain(%swift.refcounted* %A)
  tail call %swift.refcounted* @swift_retain(%swift.refcounted* %A)
  call void @user(%swift.refcounted* %A)
  tail call %swift.refcounted* @swift_retain(%swift.refcounted* %A)
  br label %bb3

bb2:
  call void @user(%swift.refcounted* %A)
  tail call %swift.refcounted* @swift_retain(%swift.refcounted* %A)
  call void @user(%swift.refcounted* %A)
  br label %bb3

bb3:
  tail call %swift.refcounted* @swift_retain(%swift.refcounted* %A)
  ret %swift.refcounted* %A
}

; CHECK-LABEL: define{{( protected)?}} %swift.refcounted* @swift_contractReleaseNInterleavedWithUnknown(%swift.refcounted* %A) {
; CHECK: bb1:
; CHECK-NEXT: @swift_release(
; CHECK-NEXT: @user
; CHECK-NEXT: @noread_user
; CHECK-NEXT: @swift_release_n
; CHECK-NEXT: @user
; CHECK-NEXT: br label %bb3
define %swift.refcounted* @swift_contractReleaseNInterleavedWithUnknown(%swift.refcounted* %A) {
entry:
  br i1 undef, label %bb1, label %bb2

bb1:
  tail call void @swift_release(%swift.refcounted* %A)
  call void @user(%swift.refcounted* %A)
  tail call void @swift_release(%swift.refcounted* %A)
  call void @noread_user(%swift.refcounted* %A)
  tail call void @swift_release(%swift.refcounted* %A)
  call void @user(%swift.refcounted* %A)
  br label %bb3

bb2:
  call void @user(%swift.refcounted* %A)
  tail call void @swift_release(%swift.refcounted* %A)
  call void @user(%swift.refcounted* %A)
  br label %bb3

bb3:
  tail call void @swift_release(%swift.refcounted* %A)
  ret %swift.refcounted* %A
}

; CHECK-LABEL: define{{( protected)?}} %swift.refcounted* @swift_contractRetainReleaseNInterleavedWithUnknown(%swift.refcounted* %A) {
; CHECK: bb1:
; CHECK-NEXT: tail call %swift.refcounted* @swift_retain(%swift.refcounted* %A)
; CHECK-NEXT: call void @user(%swift.refcounted* %A)
; CHECK-NEXT: call void @noread_user(%swift.refcounted* %A)
; CHECK-NEXT: tail call void @swift_release_n(%swift.refcounted* %A, i32 2)
; CHECK-NEXT: call void @user(%swift.refcounted* %A)
; CHECK-NEXT: tail call %swift.refcounted* @swift_retain_n(%swift.refcounted* %A, i32 2)
; CHECK-NEXT: tail call void @swift_release(%swift.refcounted* %A)
; CHECK-NEXT: call void @user(%swift.refcounted* %A)
; CHECK-NEXT: call void @noread_user(%swift.refcounted* %A)
; CHECK-NEXT: tail call void @swift_release_n(%swift.refcounted* %A, i32 2)
; CHECK-NEXT: call void @user(%swift.refcounted* %A)
; CHECK-NEXT: br label %bb3
; CHECK: bb2:
; CHECK-NEXT: call void @user(%swift.refcounted* %A)
; CHECK-NEXT: call void @swift_release(%swift.refcounted* %A)
; CHECK-NEXT: call void @user(%swift.refcounted* %A)
; CHECK-NEXT: br label %bb3

; CHECK: bb3:
; CHECK-NEXT: tail call void @swift_release(%swift.refcounted* %A)
; CHECK-NEXT: ret %swift.refcounted* %A
define %swift.refcounted* @swift_contractRetainReleaseNInterleavedWithUnknown(%swift.refcounted* %A) {
entry:
  br i1 undef, label %bb1, label %bb2

bb1:
  tail call %swift.refcounted* @swift_retain(%swift.refcounted* %A)
  call void @user(%swift.refcounted* %A)
  tail call void @swift_release(%swift.refcounted* %A)
  call void @noread_user(%swift.refcounted* %A)
  tail call void @swift_release(%swift.refcounted* %A)
  call void @user(%swift.refcounted* %A)
  tail call %swift.refcounted* @swift_retain(%swift.refcounted* %A)
  tail call %swift.refcounted* @swift_retain(%swift.refcounted* %A)
  tail call void @swift_release(%swift.refcounted* %A)
  call void @user(%swift.refcounted* %A)
  tail call void @swift_release(%swift.refcounted* %A)
  call void @noread_user(%swift.refcounted* %A)
  tail call void @swift_release(%swift.refcounted* %A)
  call void @user(%swift.refcounted* %A)

  br label %bb3

bb2:
  call void @user(%swift.refcounted* %A)
  tail call void @swift_release(%swift.refcounted* %A)
  call void @user(%swift.refcounted* %A)
  br label %bb3

bb3:
  tail call void @swift_release(%swift.refcounted* %A)
  ret %swift.refcounted* %A
}

; CHECK-LABEL: define{{( protected)?}} %swift.refcounted* @swift_contractUnknownObjectRetainNInterleavedWithUnknown(%swift.refcounted* %A) {
; CHECK: bb1:
; CHECK: tail call %swift.refcounted* @swift_unknownObjectRetain(%swift.refcounted* %A)
; CHECK-NEXT: call void @user(%swift.refcounted* %A)
; CHECK-NEXT: tail call %swift.refcounted* @swift_unknownObjectRetain_n(%swift.refcounted* %A, i32 3)
; CHECK-NEXT: call void @noread_user(%swift.refcounted* %A)
; CHECK-NEXT: call void @noread_user(%swift.refcounted* %A)
; CHECK-NEXT: call void @user(%swift.refcounted* %A)
; CHECK-NEXT: tail call %swift.refcounted* @swift_unknownObjectRetain_n(%swift.refcounted* %A, i32 2)
; CHECK-NEXT: call void @user(%swift.refcounted* %A)
; CHECK-NEXT: tail call %swift.refcounted* @swift_unknownObjectRetain(%swift.refcounted* %A)
; CHECK-NEXT: br label %bb3

; CHECK: bb2:
; CHECK-NEXT: call void @user(%swift.refcounted* %A)
; CHECK-NEXT: tail call %swift.refcounted* @swift_unknownObjectRetain(%swift.refcounted* %A)
; CHECK-NEXT: call void @user(%swift.refcounted* %A)
; CHECK-NEXT: br label %bb3

; CHECK: bb3:
; CHECK-NEXT: tail call %swift.refcounted* @swift_unknownObjectRetain(%swift.refcounted* %A)
; CHECK-NEXT: ret %swift.refcounted* %A
define %swift.refcounted* @swift_contractUnknownObjectRetainNInterleavedWithUnknown(%swift.refcounted* %A) {
entry:
  br i1 undef, label %bb1, label %bb2

bb1:
  tail call %swift.refcounted* @swift_unknownObjectRetain(%swift.refcounted* %A)
  call void @user(%swift.refcounted* %A)
  tail call %swift.refcounted* @swift_unknownObjectRetain(%swift.refcounted* %A)
  call void @noread_user(%swift.refcounted* %A)
  tail call %swift.refcounted* @swift_unknownObjectRetain(%swift.refcounted* %A)
  call void @noread_user(%swift.refcounted* %A)
  tail call %swift.refcounted* @swift_unknownObjectRetain(%swift.refcounted* %A)
  call void @user(%swift.refcounted* %A)
  tail call %swift.refcounted* @swift_unknownObjectRetain(%swift.refcounted* %A)
  tail call %swift.refcounted* @swift_unknownObjectRetain(%swift.refcounted* %A)
  call void @user(%swift.refcounted* %A)
  tail call %swift.refcounted* @swift_unknownObjectRetain(%swift.refcounted* %A)
  br label %bb3

bb2:
  call void @user(%swift.refcounted* %A)
  tail call %swift.refcounted* @swift_unknownObjectRetain(%swift.refcounted* %A)
  call void @user(%swift.refcounted* %A)
  br label %bb3

bb3:
  tail call %swift.refcounted* @swift_unknownObjectRetain(%swift.refcounted* %A)
  ret %swift.refcounted* %A
}

; CHECK-LABEL: define{{( protected)?}} %swift.refcounted* @swift_contractUnknownObjectReleaseNInterleavedWithUnknown(%swift.refcounted* %A) {
; CHECK: bb1:
; CHECK-NEXT: @swift_unknownObjectRelease(
; CHECK-NEXT: @user
; CHECK-NEXT: @noread_user
; CHECK-NEXT: @swift_unknownObjectRelease_n
; CHECK-NEXT: @user
; CHECK-NEXT: br label %bb3
define %swift.refcounted* @swift_contractUnknownObjectReleaseNInterleavedWithUnknown(%swift.refcounted* %A) {
entry:
  br i1 undef, label %bb1, label %bb2

bb1:
  tail call void @swift_unknownObjectRelease(%swift.refcounted* %A)
  call void @user(%swift.refcounted* %A)
  tail call void @swift_unknownObjectRelease(%swift.refcounted* %A)
  call void @noread_user(%swift.refcounted* %A)
  tail call void @swift_unknownObjectRelease(%swift.refcounted* %A)
  call void @user(%swift.refcounted* %A)
  br label %bb3

bb2:
  call void @user(%swift.refcounted* %A)
  tail call void @swift_unknownObjectRelease(%swift.refcounted* %A)
  call void @user(%swift.refcounted* %A)
  br label %bb3

bb3:
  tail call void @swift_unknownObjectRelease(%swift.refcounted* %A)
  ret %swift.refcounted* %A
}

; CHECK-LABEL: define{{( protected)?}} %swift.refcounted* @swift_contractUnknownObjectRetainReleaseNInterleavedWithUnknown(%swift.refcounted* %A) {
; CHECK: bb1:
; CHECK-NEXT: tail call %swift.refcounted* @swift_unknownObjectRetain(%swift.refcounted* %A)
; CHECK-NEXT: call void @user(%swift.refcounted* %A)
; CHECK-NEXT: call void @noread_user(%swift.refcounted* %A)
; CHECK-NEXT: tail call void @swift_unknownObjectRelease_n(%swift.refcounted* %A, i32 2)
; CHECK-NEXT: call void @user(%swift.refcounted* %A)
; CHECK-NEXT: tail call %swift.refcounted* @swift_unknownObjectRetain_n(%swift.refcounted* %A, i32 2)
; CHECK-NEXT: tail call void @swift_unknownObjectRelease(%swift.refcounted* %A)
; CHECK-NEXT: call void @user(%swift.refcounted* %A)
; CHECK-NEXT: call void @noread_user(%swift.refcounted* %A)
; CHECK-NEXT: tail call void @swift_unknownObjectRelease_n(%swift.refcounted* %A, i32 2)
; CHECK-NEXT: call void @user(%swift.refcounted* %A)
; CHECK-NEXT: br label %bb3
; CHECK: bb2:
; CHECK-NEXT: call void @user(%swift.refcounted* %A)
; CHECK-NEXT: call void @swift_unknownObjectRelease(%swift.refcounted* %A)
; CHECK-NEXT: call void @user(%swift.refcounted* %A)
; CHECK-NEXT: br label %bb3

; CHECK: bb3:
; CHECK-NEXT: tail call void @swift_unknownObjectRelease(%swift.refcounted* %A)
; CHECK-NEXT: ret %swift.refcounted* %A
define %swift.refcounted* @swift_contractUnknownObjectRetainReleaseNInterleavedWithUnknown(%swift.refcounted* %A) {
entry:
  br i1 undef, label %bb1, label %bb2

bb1:
  tail call %swift.refcounted* @swift_unknownObjectRetain(%swift.refcounted* %A)
  call void @user(%swift.refcounted* %A)
  tail call void @swift_unknownObjectRelease(%swift.refcounted* %A)
  call void @noread_user(%swift.refcounted* %A)
  tail call void @swift_unknownObjectRelease(%swift.refcounted* %A)
  call void @user(%swift.refcounted* %A)
  tail call %swift.refcounted* @swift_unknownObjectRetain(%swift.refcounted* %A)
  tail call %swift.refcounted* @swift_unknownObjectRetain(%swift.refcounted* %A)
  tail call void @swift_unknownObjectRelease(%swift.refcounted* %A)
  call void @user(%swift.refcounted* %A)
  tail call void @swift_unknownObjectRelease(%swift.refcounted* %A)
  call void @noread_user(%swift.refcounted* %A)
  tail call void @swift_unknownObjectRelease(%swift.refcounted* %A)
  call void @user(%swift.refcounted* %A)
  br label %bb3

bb2:
  call void @user(%swift.refcounted* %A)
  tail call void @swift_unknownObjectRelease(%swift.refcounted* %A)
  call void @user(%swift.refcounted* %A)
  br label %bb3

bb3:
  tail call void @swift_unknownObjectRelease(%swift.refcounted* %A)
  ret %swift.refcounted* %A
}


; CHECK-LABEL: define{{( protected)?}} %swift.bridge* @swift_contractBridgeRetainWithBridge(%swift.bridge* %A) {
; CHECK: bb1:
; CHECK-NEXT: [[RET0:%.+]] = tail call %swift.bridge* @swift_bridgeObjectRetain_n(%swift.bridge* %A, i32 2)
; CHECK-NEXT: tail call void @swift_bridgeObjectRelease(%swift.bridge* [[RET0:%.+]])
; CHECK-NEXT: tail call void @swift_bridgeObjectRelease(%swift.bridge* %A)
; CHECK-NEXT: ret %swift.bridge* %A
define %swift.bridge* @swift_contractBridgeRetainWithBridge(%swift.bridge* %A) {
bb1:
  %0 = tail call %swift.bridge* @swift_bridgeObjectRetain(%swift.bridge* %A)
  %1 = tail call %swift.bridge* @swift_bridgeObjectRetain(%swift.bridge* %A)
  tail call void @swift_bridgeObjectRelease(%swift.bridge* %1)
  tail call void @swift_bridgeObjectRelease(%swift.bridge* %A)
  ret %swift.bridge* %A
}

; CHECK-LABEL: define{{( protected)?}} %swift.bridge* @swift_contractBridgeRetainReleaseNInterleavedWithBridge(%swift.bridge* %A) {
; CHECK: bb1:
; CHECK-NEXT: [[RET0:%.+]] = tail call %swift.bridge* @swift_bridgeObjectRetain(%swift.bridge* %A)
; CHECK-NEXT: call void @user_bridged(%swift.bridge* %A)
; CHECK-NEXT: call void @noread_user_bridged(%swift.bridge* %A)
; CHECK-NEXT: tail call void @swift_bridgeObjectRelease_n(%swift.bridge* %A, i32 2)
; CHECK-NEXT: call void @user_bridged(%swift.bridge* %A)
; CHECK-NEXT: [[RET1:%.+]] = tail call %swift.bridge* @swift_bridgeObjectRetain_n(%swift.bridge* %A, i32 2)
; CHECK-NEXT: tail call void @swift_bridgeObjectRelease(%swift.bridge* %A)
; CHECK-NEXT: call void @user_bridged(%swift.bridge* %A)
; CHECK-NEXT: call void @noread_user_bridged(%swift.bridge* %A)
; CHECK-NEXT: tail call void @swift_bridgeObjectRelease_n(%swift.bridge* %A, i32 2)
; CHECK-NEXT: call void @user_bridged(%swift.bridge* %A)
; CHECK-NEXT: br label %bb3
; CHECK: bb2:
; CHECK-NEXT: call void @user_bridged(%swift.bridge* %A)
; CHECK-NEXT: call void @swift_bridgeObjectRelease(%swift.bridge* %A)
; CHECK-NEXT: call void @user_bridged(%swift.bridge* %A)
; CHECK-NEXT: br label %bb3

; CHECK: bb3:
; CHECK-NEXT: tail call void @swift_bridgeObjectRelease(%swift.bridge* %A)
; CHECK-NEXT: ret %swift.bridge* %A
define %swift.bridge* @swift_contractBridgeRetainReleaseNInterleavedWithBridge(%swift.bridge* %A) {
entry:
  br i1 undef, label %bb1, label %bb2

bb1:
  tail call %swift.bridge* @swift_bridgeObjectRetain(%swift.bridge* %A)
  call void @user_bridged(%swift.bridge* %A)
  tail call void @swift_bridgeObjectRelease(%swift.bridge* %A)
  call void @noread_user_bridged(%swift.bridge* %A)
  tail call void @swift_bridgeObjectRelease(%swift.bridge* %A)
  call void @user_bridged(%swift.bridge* %A)
  tail call %swift.bridge* @swift_bridgeObjectRetain(%swift.bridge* %A)
  tail call %swift.bridge* @swift_bridgeObjectRetain(%swift.bridge* %A)
  tail call void @swift_bridgeObjectRelease(%swift.bridge* %A)
  call void @user_bridged(%swift.bridge* %A)
  tail call void @swift_bridgeObjectRelease(%swift.bridge* %A)
  call void @noread_user_bridged(%swift.bridge* %A)
  tail call void @swift_bridgeObjectRelease(%swift.bridge* %A)
  call void @user_bridged(%swift.bridge* %A)
  br label %bb3

bb2:
  call void @user_bridged(%swift.bridge* %A)
  tail call void @swift_bridgeObjectRelease(%swift.bridge* %A)
  call void @user_bridged(%swift.bridge* %A)
  br label %bb3

bb3:
  tail call void @swift_bridgeObjectRelease(%swift.bridge* %A)
  ret %swift.bridge* %A
}

!llvm.dbg.cu = !{!1}
!llvm.module.flags = !{!4}

!0 = !DILocation(line: 0, scope: !3)
!1 = distinct !DICompileUnit(language: DW_LANG_Swift, file: !2)
!2 = !DIFile(filename: "contract.swift", directory: "")
!3 = distinct !DISubprogram(name: "_", scope: !1, file: !2, type: !DISubroutineType(types: !{}))
!4 = !{i32 1, !"Debug Info Version", i32 3}

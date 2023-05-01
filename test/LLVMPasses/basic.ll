; RUN: %swift-llvm-opt -swift-llvm-arc-optimize %s | %FileCheck %s

; Use this testfile to check if the `swift-frontend -swift-dependency-tool` option works.
; RUN: %swift_frontend_plain -swift-llvm-opt -swift-llvm-arc-optimize %s | %FileCheck %s

target datalayout = "e-p:64:64:64-S128-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f16:16:16-f32:32:32-f64:64:64-f128:128:128-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64"
target triple = "x86_64-apple-macosx10.9"

%swift.refcounted = type { %swift.heapmetadata*, i64 }
%swift.heapmetadata = type { i64 (%swift.refcounted*)*, i64 (%swift.refcounted*)* }
%objc_object = type opaque
%swift.bridge = type opaque

declare %swift.refcounted* @swift_unknownObjectRetain(%swift.refcounted* returned)
declare void @swift_unknownObjectRelease(%swift.refcounted*)
declare i8* @llvm.objc.retain(i8*)
declare void @llvm.objc.release(i8*)
declare %swift.refcounted* @swift_allocObject(%swift.heapmetadata* , i64, i64) nounwind
declare void @swift_release(%swift.refcounted* nocapture)
declare %swift.refcounted* @swift_retain(%swift.refcounted* returned) nounwind
declare %swift.bridge* @swift_bridgeObjectRetain(%swift.bridge*)
declare void @swift_bridgeObjectRelease(%swift.bridge*)
declare %swift.refcounted* @swift_retainUnowned(%swift.refcounted* returned)

declare void @user(%swift.refcounted *) nounwind
declare void @user_objc(i8*) nounwind
declare void @unknown_func()

define private void @__swift_fixLifetime(%swift.refcounted*) noinline nounwind {
entry:
  ret void
}

; CHECK-LABEL: @trivial_objc_canonicalization(
; CHECK-NEXT: entry:
; CHECK-NEXT: [[RET0:%.+]] = bitcast i8* %O to i8*
; CHECK-NEXT: [[RET1:%.+]] = tail call i8* @llvm.objc.retain(i8* [[RET0:%.+]])
; CHECK-NEXT: call void @user_objc(i8* [[RET0:%.+]])
; CHECK-NEXT: ret void

define void @trivial_objc_canonicalization(i8* %O) {
entry:
  %0 = bitcast i8* %O to i8*
  %1 = tail call i8* @llvm.objc.retain(i8* %0)
  call void @user_objc(i8* %1) nounwind
  ret void
}

; CHECK-LABEL: @trivial_retain_release(
; CHECK-NEXT: entry:
; CHECK-NEXT: call void @user
; CHECK-NEXT: ret void

define void @trivial_retain_release(%swift.refcounted* %P, i8* %O, %swift.bridge * %B) {
entry:
  tail call %swift.refcounted* @swift_retain(%swift.refcounted* %P)
  tail call void @swift_release(%swift.refcounted* %P) nounwind
  tail call %swift.refcounted* @swift_unknownObjectRetain(%swift.refcounted* %P)
  tail call void @swift_unknownObjectRelease(%swift.refcounted* %P)
  tail call i8* @llvm.objc.retain(i8* %O)
  tail call void @llvm.objc.release(i8* %O)
  %v = tail call %swift.bridge* @swift_bridgeObjectRetain(%swift.bridge* %B)
  tail call void @swift_bridgeObjectRelease(%swift.bridge* %v)
  call void @user(%swift.refcounted* %P) nounwind
  ret void
}

; CHECK-LABEL: @trivial_retain_release_with_rcidentity(
; CHECK-NEXT: entry:
; CHECK-NEXT: [[RET0:%.+]] = bitcast %swift.refcounted* %P to %swift.refcounted*
; CHECK-NEXT: [[RET1:%.+]] = bitcast %swift.refcounted* %P to %swift.refcounted*
; CHECK-NEXT: [[RET2:%.+]] = bitcast i8* %O to i8*
; CHECK-NEXT: call void @user
; CHECK-NEXT: ret void

define void @trivial_retain_release_with_rcidentity(%swift.refcounted* %P, i8* %O, %swift.bridge * %B) {
entry:
  tail call %swift.refcounted* @swift_retain(%swift.refcounted* %P)
  %1 = bitcast %swift.refcounted* %P to %swift.refcounted*
  tail call void @swift_release(%swift.refcounted* %1) nounwind
  tail call %swift.refcounted* @swift_unknownObjectRetain(%swift.refcounted* %P)
  %3 = bitcast %swift.refcounted* %P to %swift.refcounted*
  tail call void @swift_unknownObjectRelease(%swift.refcounted* %3)
  tail call i8* @llvm.objc.retain(i8* %O)
  %5 = bitcast i8* %O to i8*
  tail call void @llvm.objc.release(i8* %5)
  call void @user(%swift.refcounted* %P) nounwind
  ret void
}

; retain_motion1 - This shows motion of a retain across operations that can't
; release an object.  Release motion can't zap this.

; CHECK-LABEL: @retain_motion1(
; CHECK-NEXT: bitcast
; CHECK-NEXT: store i32
; CHECK-NEXT: ret void


define void @retain_motion1(%swift.refcounted* %A) {
  tail call %swift.refcounted* @swift_retain(%swift.refcounted* %A)
  %B = bitcast %swift.refcounted* %A to i32*
  store i32 42, i32* %B
  tail call void @swift_release(%swift.refcounted* %A) nounwind
  ret void
}

; rdar://11583269 - Optimize out objc_retain/release(null)

; CHECK-LABEL: @objc_retain_release_null(
; CHECK-NEXT: entry:
; CHECK-NEXT: ret void

define void @objc_retain_release_null() {
entry:
  tail call void @llvm.objc.release(i8* null) nounwind
  tail call i8* @llvm.objc.retain(i8* null)
  ret void
}

; CHECK-LABEL: @swiftunknown_retain_release_null(
; CHECK-NEXT: entry:
; CHECK-NEXT: ret void

define void @swiftunknown_retain_release_null() {
entry:
  tail call void @swift_unknownObjectRelease(%swift.refcounted* null)
  tail call %swift.refcounted* @swift_unknownObjectRetain(%swift.refcounted* null) nounwind
  ret void
}

; rdar://11583269 - Useless objc_retain/release optimization.

; CHECK-LABEL: @objc_retain_release_opt(
; CHECK-NEXT: store i32 42
; CHECK-NEXT: ret void

define void @objc_retain_release_opt(i8* %P, i32* %IP) {
  tail call i8* @llvm.objc.retain(i8* %P) nounwind
  store i32 42, i32* %IP
  tail call void @llvm.objc.release(i8* %P) nounwind
  ret void
}

; CHECK-LABEL: define{{( protected)?}} void @swift_fixLifetimeTest
; CHECK: swift_retain
; CHECK: swift_fixLifetime
; CHECK: swift_release
define void @swift_fixLifetimeTest(%swift.refcounted* %A) {
  tail call %swift.refcounted* @swift_retain(%swift.refcounted* %A)
  call void @user(%swift.refcounted* %A) nounwind
  call void @__swift_fixLifetime(%swift.refcounted* %A)
  tail call void @swift_release(%swift.refcounted* %A) nounwind
  ret void
}

; CHECK-LABEL: @move_retain_across_unknown_retain
; CHECK-NOT: swift_retain
; CHECK: swift_unknownObjectRetain
; CHECK-NOT: swift_release
; CHECK: ret
define void @move_retain_across_unknown_retain(%swift.refcounted* %A, %swift.refcounted* %B) {
  tail call %swift.refcounted* @swift_retain(%swift.refcounted* %A)
  tail call %swift.refcounted* @swift_unknownObjectRetain(%swift.refcounted* %B)
  tail call void @swift_release(%swift.refcounted* %A) nounwind
  ret void
}

; CHECK-LABEL: @move_retain_across_objc_retain
; CHECK-NOT: swift_retain
; CHECK: llvm.objc.retain
; CHECK-NOT: swift_release
; CHECK: ret
define void @move_retain_across_objc_retain(%swift.refcounted* %A, i8* %B) {
  tail call %swift.refcounted* @swift_retain(%swift.refcounted* %A)
  tail call i8* @llvm.objc.retain(i8* %B)
  tail call void @swift_release(%swift.refcounted* %A) nounwind
  ret void
}

; CHECK-LABEL: @move_retain_across_load
; CHECK-NOT: swift_retain
; CHECK-NOT: swift_release
; CHECK: ret
define i32 @move_retain_across_load(%swift.refcounted* %A, i32* %ptr) {
  tail call %swift.refcounted* @swift_retain(%swift.refcounted* %A)
  %val = load i32, i32* %ptr
  tail call void @swift_release(%swift.refcounted* %A) nounwind
  ret i32 %val
}

; CHECK-LABEL: @move_retain_but_not_release_across_objc_fix_lifetime
; CHECK: call void @__swift_fixLifetime
; CHECK-NEXT: tail call %swift.refcounted* @swift_retain
; CHECK-NEXT: call void @user
; CHECK-NEXT: call void @__swift_fixLifetime
; CHECK-NEXT: call void @swift_release
; CHECK-NEXT: ret
define void @move_retain_but_not_release_across_objc_fix_lifetime(%swift.refcounted* %A) {
  tail call %swift.refcounted* @swift_retain(%swift.refcounted* %A)
  call void @__swift_fixLifetime(%swift.refcounted* %A) nounwind
  call void @user(%swift.refcounted* %A) nounwind
  call void @__swift_fixLifetime(%swift.refcounted* %A) nounwind
  tail call void @swift_release(%swift.refcounted* %A) nounwind
  ret void
}

; CHECK-LABEL: @optimize_retain_unowned
; CHECK-NEXT: bitcast
; CHECK-NEXT: load
; CHECK-NEXT: add
; CHECK-NEXT: load
; CHECK-NEXT: call void @swift_checkUnowned
; CHECK-NEXT: ret
define void @optimize_retain_unowned(%swift.refcounted* %A) {
  tail call %swift.refcounted* @swift_retainUnowned(%swift.refcounted* %A)
  %value = bitcast %swift.refcounted* %A to i64*

  ; loads from the %A and speculatively executable instructions
  %L1 = load i64, i64* %value, align 8
  %R1 = add i64 %L1, 1
  %L2 = load i64, i64* %value, align 8

  tail call void @swift_release(%swift.refcounted* %A)
  ret void
}

; CHECK-LABEL: @dont_optimize_retain_unowned
; CHECK-NEXT: call %swift.refcounted* @swift_retainUnowned
; CHECK-NEXT: bitcast
; CHECK-NEXT: load
; CHECK-NEXT: load
; CHECK-NEXT: call void @swift_release
; CHECK-NEXT: ret
define void @dont_optimize_retain_unowned(%swift.refcounted* %A) {
  tail call %swift.refcounted* @swift_retainUnowned(%swift.refcounted* %A)
  %value = bitcast %swift.refcounted* %A to i64**

  %L1 = load i64*, i64** %value, align 8
  ; Use of a potential garbage address from a load of %A.
  %L2 = load i64, i64* %L1, align 8

  tail call void @swift_release(%swift.refcounted* %A)
  ret void
}

; CHECK-LABEL: @dont_optimize_retain_unowned2
; CHECK-NEXT: call %swift.refcounted* @swift_retainUnowned
; CHECK-NEXT: store
; CHECK-NEXT: call void @swift_release
; CHECK-NEXT: ret
define void @dont_optimize_retain_unowned2(%swift.refcounted* %A, i32* %B) {
  tail call %swift.refcounted* @swift_retainUnowned(%swift.refcounted* %A)

  ; store to an unknown address
  store i32 42, i32* %B

  tail call void @swift_release(%swift.refcounted* %A)
  ret void
}

; CHECK-LABEL: @dont_optimize_retain_unowned3
; CHECK-NEXT: call %swift.refcounted* @swift_retainUnowned
; CHECK-NEXT: call void @unknown_func
; CHECK-NEXT: call void @swift_release
; CHECK-NEXT: ret
define void @dont_optimize_retain_unowned3(%swift.refcounted* %A) {
  tail call %swift.refcounted* @swift_retainUnowned(%swift.refcounted* %A)

  ; call of an unknown function
  call void @unknown_func()

  tail call void @swift_release(%swift.refcounted* %A)
  ret void
}

; CHECK-LABEL: @dont_optimize_retain_unowned4
; CHECK-NEXT: call %swift.refcounted* @swift_retainUnowned
; CHECK-NEXT: call %swift.refcounted* @swift_retain
; CHECK-NEXT: call void @swift_release
; CHECK-NEXT: ret
define void @dont_optimize_retain_unowned4(%swift.refcounted* %A, %swift.refcounted* %B) {
  tail call %swift.refcounted* @swift_retainUnowned(%swift.refcounted* %A)

  ; retain of an unknown reference (%B could be equal to %A)
  tail call %swift.refcounted* @swift_retain(%swift.refcounted* %B)

  tail call void @swift_release(%swift.refcounted* %A)
  ret void
}

; CHECK-LABEL: @remove_redundant_check_unowned
; CHECK-NEXT: bitcast
; CHECK-NEXT: load
; CHECK-NEXT: call void @swift_checkUnowned
; CHECK-NEXT: load
; CHECK-NEXT: store
; CHECK-NEXT: load
; CHECK-NEXT: ret
define void @remove_redundant_check_unowned(%swift.refcounted* %A, %swift.refcounted* %B, i64* %C) {
  tail call %swift.refcounted* @swift_retainUnowned(%swift.refcounted* %A)
  %addr = bitcast %swift.refcounted* %A to i64*
  %L1 = load i64, i64* %addr, align 8
  tail call void @swift_release(%swift.refcounted* %A)

  ; Instructions which cannot do a release.
  %L2 = load i64, i64* %C, align 8
  store i64 42, i64* %C

  tail call %swift.refcounted* @swift_retainUnowned(%swift.refcounted* %A)
  %L3 = load i64, i64* %addr, align 8
  tail call void @swift_release(%swift.refcounted* %A)
  ret void
}

; CHECK-LABEL: @dont_remove_redundant_check_unowned
; CHECK-NEXT: bitcast
; CHECK-NEXT: load
; CHECK-NEXT: call void @swift_checkUnowned
; CHECK-NEXT: call void @unknown_func
; CHECK-NEXT: load
; CHECK-NEXT: call void @swift_checkUnowned
; CHECK-NEXT: ret
define void @dont_remove_redundant_check_unowned(%swift.refcounted* %A, %swift.refcounted* %B, i64* %C) {
  tail call %swift.refcounted* @swift_retainUnowned(%swift.refcounted* %A)
  %addr = bitcast %swift.refcounted* %A to i64*
  %L1 = load i64, i64* %addr, align 8
  tail call void @swift_release(%swift.refcounted* %A)

  ; Could do a release of %A
  call void @unknown_func()

  tail call %swift.refcounted* @swift_retainUnowned(%swift.refcounted* %A)
  %L3 = load i64, i64* %addr, align 8
  tail call void @swift_release(%swift.refcounted* %A)
  ret void
}

; CHECK-LABEL: @unknown_retain_promotion
; CHECK-NEXT: swift_retain
; CHECK-NEXT: swift_retain
; CHECK-NEXT: swift_retain
; CHECK-NEXT: ret
define void @unknown_retain_promotion(%swift.refcounted* %A) {
  tail call %swift.refcounted* @swift_unknownObjectRetain(%swift.refcounted* %A)
  tail call %swift.refcounted* @swift_unknownObjectRetain(%swift.refcounted* %A)
  tail call %swift.refcounted* @swift_retain(%swift.refcounted* %A)
  ret void
}

; CHECK-LABEL: @unknown_release_promotion
; CHECK-NEXT: swift_release
; CHECK-NEXT: swift_release
; CHECK-NEXT: swift_release
; CHECK-NEXT: ret
define void @unknown_release_promotion(%swift.refcounted* %A) {
  tail call void @swift_unknownObjectRelease(%swift.refcounted* %A)
  tail call void @swift_unknownObjectRelease(%swift.refcounted* %A)
  tail call void @swift_release(%swift.refcounted* %A)
  ret void
}

; CHECK-LABEL: @unknown_retain_nopromotion
; CHECK: bb1
; CHECK-NOT: swift_retain
; CHECK: ret
define void @unknown_retain_nopromotion(%swift.refcounted* %A) {
  tail call %swift.refcounted* @swift_retain(%swift.refcounted* %A)
  br label %bb1
bb1:
  tail call %swift.refcounted* @swift_unknownObjectRetain(%swift.refcounted* %A)
  ret void
}

; CHECK-LABEL: @releasemotion_forwarding
; CHECK-NOT: swift_retain
; CHECK-NOT: swift_release
; CHECK: call void @user(%swift.refcounted* %P)
; CHECK: ret
define void @releasemotion_forwarding(%swift.refcounted* %P, i8* %O, %swift.bridge* %B) {
entry:
  %res = tail call %swift.refcounted* @swift_retain(%swift.refcounted* %P)
  tail call void @swift_release(%swift.refcounted* %res) nounwind
  call void @user(%swift.refcounted* %res) nounwind
  ret void
}

; CHECK-LABEL: @retainmotion_forwarding
; CHECK: store %swift.refcounted* %P, %swift.refcounted** %R, align 4
; CHECK-NOT: swift_retain
; CHECK-NOT: swift_release
; CHECK: ret
define void @retainmotion_forwarding(%swift.refcounted* %P, %swift.refcounted** %R, %swift.bridge* %B) {
entry:
  %res = tail call %swift.refcounted* @swift_retain(%swift.refcounted* %P)
  store %swift.refcounted* %res, %swift.refcounted** %R, align 4
  call void @swift_bridgeObjectRelease(%swift.bridge* %B)
  tail call void @swift_release(%swift.refcounted* %res) nounwind
  ret void
}

; CHECK-LABEL: @unknownreleasemotion_forwarding
; CHECK-NOT: swift_unknownObjectRetain
; CHECK-NOT: swift_unknownObjectRelease
; CHECK: call void @user(%swift.refcounted* %P)
; CHECK: ret
define void @unknownreleasemotion_forwarding(%swift.refcounted* %P, i8* %O, %swift.bridge* %B) {
entry:
  %res = tail call %swift.refcounted* @swift_unknownObjectRetain(%swift.refcounted* %P)
  tail call void @swift_unknownObjectRelease(%swift.refcounted* %res) nounwind
  call void @user(%swift.refcounted* %res) nounwind
  ret void
}

; CHECK-LABEL: @unknownretainmotion_forwarding
; CHECK: store %swift.refcounted* %P, %swift.refcounted** %R, align 4
; CHECK-NOT: swift_unknownObjectRetain
; CHECK-NOT: swift_unknownObjectRelease
; CHECK: ret
define void @unknownretainmotion_forwarding(%swift.refcounted* %P, %swift.refcounted** %R, %swift.bridge* %B) {
entry:
  %res = tail call %swift.refcounted* @swift_unknownObjectRetain(%swift.refcounted* %P)
  store %swift.refcounted* %res, %swift.refcounted** %R, align 4
  call void @swift_bridgeObjectRelease(%swift.bridge* %B)
  tail call void @swift_unknownObjectRelease(%swift.refcounted* %res) nounwind
  ret void
}

!llvm.dbg.cu = !{!1}
!llvm.module.flags = !{!4}

!0 = !DILocation(line: 0, scope: !3)
!1 = distinct !DICompileUnit(language: DW_LANG_Swift, file: !2)
!2 = !DIFile(filename: "basic.swift", directory: "")
!3 = distinct !DISubprogram(name: "_", scope: !1, file: !2, type: !DISubroutineType(types: !{}))
!4 = !{i32 1, !"Debug Info Version", i32 3}

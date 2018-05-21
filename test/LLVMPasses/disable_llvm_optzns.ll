; RUN: %target-swift-frontend -emit-ir -O -disable-llvm-optzns %s | %FileCheck %s
; RUN: %target-swift-frontend -emit-ir -O %s | %FileCheck -check-prefix=NEGATIVE %s

; Make sure that our optimization LLVM passes respect -disable-llvm-optzns.

target datalayout = "e-p:64:64:64-S128-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f16:16:16-f32:32:32-f64:64:64-f128:128:128-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64"
target triple = "x86_64-apple-macosx10.9"

%swift.refcounted = type { %swift.heapmetadata*, i64 }
%swift.heapmetadata = type { i64 (%swift.refcounted*)*, i64 (%swift.refcounted*)* }
%objc_object = type opaque
%swift.bridge = type opaque

declare %swift.refcounted* @swift_unknownRetain(%swift.refcounted* returned)
declare void @swift_unknownRelease(%swift.refcounted*)
declare %objc_object* @objc_retain(%objc_object*)
declare void @objc_release(%objc_object*)
declare %swift.refcounted* @swift_allocObject(%swift.heapmetadata* , i64, i64) nounwind
declare void @swift_release(%swift.refcounted* nocapture)
declare %swift.refcounted* @swift_retain(%swift.refcounted* returned) nounwind
declare %swift.bridge* @swift_bridgeObjectRetain(%swift.bridge*)
declare void @swift_bridgeObjectRelease(%swift.bridge*)
declare %swift.refcounted* @swift_retainUnowned(%swift.refcounted* returned)

declare void @user(%swift.refcounted *) nounwind
declare void @user_objc(%objc_object*) nounwind
declare void @unknown_func()

; CHECK-LABEL: @trivial_retain_release(
; CHECK-NEXT: entry:
; CHECK-NEXT: call %swift.refcounted* @swift_retain(%swift.refcounted* %P)
; CHECK-NEXT: call void @swift_release(
; CHECK-NEXT: call void @user(
; CHECK-NEXT: ret void

; NEGATIVE-LABEL: @trivial_retain_release(
; NEGATIVE-NEXT: entry:
; NEGATIVE-NEXT: call void @user(
; NEGATIVE-NEXT: ret void
define void @trivial_retain_release(%swift.refcounted* %P, %objc_object* %O, %swift.bridge * %B) {
entry:
  call %swift.refcounted* @swift_retain(%swift.refcounted* %P)
  call void @swift_release(%swift.refcounted* %P) nounwind
  call void @user(%swift.refcounted* %P) nounwind
  ret void
}

!llvm.dbg.cu = !{!1}
!llvm.module.flags = !{!4}

!0 = !DILocation(line: 0, scope: !3)
!1 = distinct !DICompileUnit(language: DW_LANG_Swift, file: !2)
!2 = !DIFile(filename: "basic.swift", directory: "")
!3 = distinct !DISubprogram(name: "_", scope: !1, file: !2, type: !DISubroutineType(types: !{}))
!4 = !{i32 1, !"Debug Info Version", i32 3}

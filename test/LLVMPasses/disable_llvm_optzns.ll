; RUN: %target-swift-frontend -emit-ir -O -disable-llvm-optzns %s | %FileCheck %s
; RUN: %target-swift-frontend -emit-ir -O %s | %FileCheck -check-prefix=NEGATIVE %s

; Make sure that our optimization LLVM passes respect -disable-llvm-optzns.

target datalayout = "e-p:64:64:64-S128-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f16:16:16-f32:32:32-f64:64:64-f128:128:128-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64"
target triple = "x86_64-apple-macosx10.9"

%swift.refcounted = type { ptr, i64 }
%swift.heapmetadata = type { ptr, ptr }
%objc_object = type opaque
%swift.bridge = type opaque

declare ptr @swift_unknownObjectRetain(ptr returned)
declare void @swift_unknownObjectRelease(ptr)
declare ptr @llvm.objc.retain(ptr)
declare void @llvm.objc.release(ptr)
declare ptr @swift_allocObject(ptr , i64, i64) nounwind
declare void @swift_release(ptr nocapture)
declare ptr @swift_retain(ptr returned) nounwind
declare ptr @swift_bridgeObjectRetain(ptr)
declare void @swift_bridgeObjectRelease(ptr)
declare ptr @swift_retainUnowned(ptr returned)

declare void @user(ptr) nounwind
declare void @user_objc(ptr) nounwind
declare void @unknown_func()

; CHECK-LABEL: @trivial_retain_release(
; CHECK-NEXT: entry:
; CHECK-NEXT: call ptr @swift_retain(ptr %P)
; CHECK-NEXT: call void @swift_release(
; CHECK-NEXT: call void @user(
; CHECK-NEXT: ret void

; NEGATIVE-LABEL: @trivial_retain_release(
; NEGATIVE-NEXT: entry:
; NEGATIVE-NEXT: call void @user(
; NEGATIVE-NEXT: ret void
define void @trivial_retain_release(ptr %P, ptr %O, ptr %B) {
entry:
  call ptr @swift_retain(ptr %P)
  call void @swift_release(ptr %P) nounwind
  call void @user(ptr %P) nounwind
  ret void
}

!llvm.dbg.cu = !{!1}
!llvm.module.flags = !{!4}

!0 = !DILocation(line: 0, scope: !3)
!1 = distinct !DICompileUnit(language: DW_LANG_Swift, file: !2)
!2 = !DIFile(filename: "basic.swift", directory: "")
!3 = distinct !DISubprogram(name: "_", scope: !1, file: !2, type: !DISubroutineType(types: !{}))
!4 = !{i32 1, !"Debug Info Version", i32 3}

; RUN: %llvm-opt -swift-arc-expand -S %s | FileCheck %s

target datalayout = "e-p:64:64:64-S128-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f16:16:16-f32:32:32-f64:64:64-f128:128:128-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64"
target triple = "x86_64-apple-macosx10.9"

%swift.refcounted = type { %swift.heapmetadata*, i64 }
%swift.heapmetadata = type { i64 (%swift.refcounted*)*, i64 (%swift.refcounted*)* }

declare %swift.refcounted* @swift_allocObject(%swift.heapmetadata* , i64, i64) nounwind
declare void @swift_release(%swift.refcounted* nocapture)
declare %swift.refcounted* @swift_retain(%swift.refcounted* ) nounwind
declare void @swift_retain_noresult(%swift.refcounted* nocapture) nounwind
declare { i64, i64, i64 } @swift_retainAndReturnThree(%swift.refcounted* , i64, i64 , i64 )

; rdar://11563395 - Synthesize calls to swift_retainAndReturnThree for better codegen

; CHECK-LABEL: @retain3_test1
; CHECK: tail call {{.*}} @swift_retainAndReturnThree
; CHECK: ret

define { i8*, i64, %swift.refcounted* } @retain3_test1(i8*, i64, %swift.refcounted*) nounwind {
entry:
  %3 = bitcast i32 0 to i32
  call void @swift_retain_noresult(%swift.refcounted* %2)
  %4 = insertvalue { i8*, i64, %swift.refcounted* } undef, i8* %0, 0
  %5 = insertvalue { i8*, i64, %swift.refcounted* } %4, i64 %1, 1
  %6 = insertvalue { i8*, i64, %swift.refcounted* } %5, %swift.refcounted* %2, 2
  ret { i8*, i64, %swift.refcounted* } %6
}

; retain3_test2 - This shows a case where something else (eg inlining an already
; optimized function) has given us a swift_retainAndReturnThree that we need to
; destructure and reassemble.
define { i8*, i64, %swift.refcounted* } @retain3_test2(i8*, i64, %swift.refcounted*) nounwind {
entry:
  call void @swift_retain_noresult(%swift.refcounted* %2)
  %3 = insertvalue { i8*, i64, %swift.refcounted* } undef, i8* %0, 0
  %4 = insertvalue { i8*, i64, %swift.refcounted* } %3, i64 %1, 1
  %5 = insertvalue { i8*, i64, %swift.refcounted* } %4, %swift.refcounted* %2, 2
  ret { i8*, i64, %swift.refcounted* } %5
}

; CHECK-LABEL: @retain3_test2
; CHECK: tail call {{.*}} @swift_retainAndReturnThree
; CHECK: ret


define { i8*, i64, %swift.refcounted* } @retain3_test3(i8*, i64, %swift.refcounted** %self.owner) nounwind {
entry:
  %2 = load %swift.refcounted*, %swift.refcounted** %self.owner, align 8
  %3 = insertvalue { i8*, i64, %swift.refcounted* } undef, i8* %0, 0
  %4 = insertvalue { i8*, i64, %swift.refcounted* } %3, i64 %1, 1
  %5 = insertvalue { i8*, i64, %swift.refcounted* } %4, %swift.refcounted* %2, 2
  call void @swift_retain_noresult(%swift.refcounted* %2)
  ret { i8*, i64, %swift.refcounted* } %5
}

; CHECK-LABEL: @retain3_test3
; CHECK: tail call {{.*}} @swift_retainAndReturnThree
; CHECK: ret

declare void @swift_fixLifetime(%swift.refcounted*)

; CHECK-LABEL: define void @fixlifetime_removal(i8*) {
; CHECK-NOT: call void swift_fixLifetime
define void @fixlifetime_removal(i8*) {
entry:
  %1 = bitcast i8* %0 to %swift.refcounted*
  call void @swift_fixLifetime(%swift.refcounted* %1)
  ret void
}


; CHECK-LABEL: define void @swift_expandNoresultTest
; CHECK: call %swift.refcounted* @swift_retain(%swift.refcounted* %A), !dbg
define void @swift_expandNoresultTest(%swift.refcounted* %A) {
  tail call void @swift_retain_noresult(%swift.refcounted* %A), !dbg !0
  tail call void @swift_release(%swift.refcounted* %A) nounwind
  ret void
}


!llvm.dbg.cu = !{!1}
!llvm.module.flags = !{!4}

!0 = !DILocation(line: 0, scope: !3)
!1 = distinct !DICompileUnit(language: DW_LANG_Swift, file: !2)
!2 = !DIFile(filename: "expand.swift", directory: "")
!3 = !DISubprogram(name: "_", scope: !1, file: !2, type: !DISubroutineType(types: !{}))
!4 = !{i32 1, !"Debug Info Version", i32 3}

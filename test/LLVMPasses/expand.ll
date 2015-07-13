; RUN: %llvm-opt -swift-arc-expand -S %s | FileCheck %s

target datalayout = "e-p:64:64:64-S128-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f16:16:16-f32:32:32-f64:64:64-f128:128:128-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64"
target triple = "x86_64-apple-macosx10.9"

%swift.refcounted = type { %swift.heapmetadata*, i64 }
%swift.heapmetadata = type { i64 (%swift.refcounted*)*, i64 (%swift.refcounted*)* }

declare %swift.refcounted* @swift_allocObject(%swift.heapmetadata* , i64, i64) nounwind
declare void @swift_release(%swift.refcounted* nocapture)
declare %swift.refcounted* @swift_retain(%swift.refcounted* ) nounwind
declare void @swift_retain_noresult(%swift.refcounted* nocapture) nounwind
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

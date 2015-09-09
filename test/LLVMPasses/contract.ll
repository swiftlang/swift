; RUN: %llvm-opt -swift-arc-contract -S %s | FileCheck %s

target datalayout = "e-p:64:64:64-S128-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f16:16:16-f32:32:32-f64:64:64-f128:128:128-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64"
target triple = "x86_64-apple-macosx10.9"

%swift.refcounted = type { %swift.heapmetadata*, i64 }
%swift.heapmetadata = type { i64 (%swift.refcounted*)*, i64 (%swift.refcounted*)* }

declare %swift.refcounted* @swift_allocObject(%swift.heapmetadata* , i64, i64) nounwind
declare void @swift_release(%swift.refcounted* nocapture)
declare %swift.refcounted* @swift_retain(%swift.refcounted* ) nounwind
declare void @swift_retain_noresult(%swift.refcounted* nocapture) nounwind
declare void @swift_fixLifetime(%swift.refcounted*)
declare void @noread_user(%swift.refcounted*) readnone
declare void @user(%swift.refcounted*)

; CHECK-LABEL: define void @fixlifetime_removal(i8*) {
; CHECK-NOT: call void swift_fixLifetime
define void @fixlifetime_removal(i8*) {
entry:
  %1 = bitcast i8* %0 to %swift.refcounted*
  call void @swift_fixLifetime(%swift.refcounted* %1)
  ret void
}


; CHECK-LABEL: define void @swift_contractNoresultTest
; CHECK: call %swift.refcounted* @swift_retain(%swift.refcounted* %A), !dbg
define void @swift_contractNoresultTest(%swift.refcounted* %A) {
  tail call void @swift_retain_noresult(%swift.refcounted* %A), !dbg !0
  tail call void @swift_release(%swift.refcounted* %A) nounwind
  ret void
}

; CHECK-LABEL: define %swift.refcounted* @swift_contractRetainN(%swift.refcounted* %A) {
; CHECK: entry:
; CHECK-NEXT: br i1 undef
; CHECK: bb1:
; CHECK-NEXT: [[RET1:%.+]] = tail call %swift.refcounted* @swift_retain_n(%swift.refcounted* %A, i32 2)
; CHECK-NEXT: call void @noread_user(%swift.refcounted* [[RET1]])
; CHECK-NEXT: call void @noread_user(%swift.refcounted* [[RET1]])
; CHECK-NEXT: br label %bb3
; CHECK: bb2:
; CHECK-NEXT: call void @noread_user(%swift.refcounted* %A)
; CHECK-NEXT: [[RET2:%.+]] = tail call %swift.refcounted* @swift_retain(%swift.refcounted* %A)
; CHECK-NEXT: call void @noread_user(%swift.refcounted* [[RET2]])
; CHECK-NEXT: br label %bb3
; CHECK: bb3:
; CHECK-NEXT: [[PHIRESULT:%.+]] = phi %swift.refcounted* [ [[RET2]], %bb2 ], [ [[RET1]], %bb1 ]
; CHECK-NEXT: [[RET3:%.+]] = tail call %swift.refcounted* @swift_retain(%swift.refcounted* [[PHIRESULT]])
; CHECK-NEXT: ret %swift.refcounted* [[RET3]]
define %swift.refcounted* @swift_contractRetainN(%swift.refcounted* %A) {
entry:
  br i1 undef, label %bb1, label %bb2

bb1:
  tail call void @swift_retain_noresult(%swift.refcounted* %A)
  call void @noread_user(%swift.refcounted* %A)
  tail call void @swift_retain_noresult(%swift.refcounted* %A)
  call void @noread_user(%swift.refcounted* %A)
  br label %bb3

bb2:
  call void @noread_user(%swift.refcounted* %A)
  tail call void @swift_retain_noresult(%swift.refcounted* %A)
  call void @noread_user(%swift.refcounted* %A)
  br label %bb3

bb3:
  tail call void @swift_retain_noresult(%swift.refcounted* %A)
  ret %swift.refcounted* %A
}

; CHECK-LABEL: define %swift.refcounted* @swift_contractReleaseN(%swift.refcounted* %A) {
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

; Make sure that we do not form retainN,releaseN over uses that may
; read the reference count of the object.

; CHECK-LABEL: define %swift.refcounted* @swift_contractRetainNWithUnknown(%swift.refcounted* %A) {
; CHECK-NOT: call %swift.refcounted* @swift_retain_n
define %swift.refcounted* @swift_contractRetainNWithUnknown(%swift.refcounted* %A) {
entry:
  br i1 undef, label %bb1, label %bb2

bb1:
  tail call void @swift_retain_noresult(%swift.refcounted* %A)
  call void @user(%swift.refcounted* %A)
  tail call void @swift_retain_noresult(%swift.refcounted* %A)
  call void @user(%swift.refcounted* %A)
  br label %bb3

bb2:
  call void @user(%swift.refcounted* %A)
  tail call void @swift_retain_noresult(%swift.refcounted* %A)
  call void @user(%swift.refcounted* %A)
  br label %bb3

bb3:
  tail call void @swift_retain_noresult(%swift.refcounted* %A)
  ret %swift.refcounted* %A
}

; CHECK-LABEL: define %swift.refcounted* @swift_contractReleaseNWithUnknown(%swift.refcounted* %A) {
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
; CHECK-LABEL: define %swift.refcounted* @swift_contractRetainNInterleavedWithUnknown(%swift.refcounted* %A) {
; CHECK: bb1:
; CHECK: [[RET1:%.*]] = tail call %swift.refcounted* @swift_retain(%swift.refcounted* %A)
; CHECK-NEXT: call void @user(%swift.refcounted* [[RET1]])
; CHECK-NEXT: [[RET2:%.*]] = tail call %swift.refcounted* @swift_retain_n(%swift.refcounted* [[RET1]], i32 3)
; CHECK-NEXT: call void @noread_user(%swift.refcounted* [[RET2]])
; CHECK-NEXT: call void @noread_user(%swift.refcounted* [[RET2]])
; CHECK-NEXT: call void @user(%swift.refcounted* [[RET2]])
; CHECK-NEXT: [[RET3:%.*]] = tail call %swift.refcounted* @swift_retain_n(%swift.refcounted* [[RET2]], i32 2)
; CHECK-NEXT: call void @user(%swift.refcounted* [[RET3]])
; CHECK-NEXT: [[RET4:%.*]] = tail call %swift.refcounted* @swift_retain(%swift.refcounted* [[RET3]])
; CHECK-NEXT: br label %bb3

; CHECK: bb2:
; CHECK-NEXT: call void @user(%swift.refcounted* %A)
; CHECK-NEXT: [[RET5:%.*]] = tail call %swift.refcounted* @swift_retain(%swift.refcounted* %A)
; CHECK-NEXT: call void @user(%swift.refcounted* [[RET5]])
; CHECK-NEXT: br label %bb3

; CHECK: bb3:
; CHECK-NEXT: [[PHIRET:%.*]] = phi %swift.refcounted* [ [[RET5]], %bb2 ], [ [[RET4]], %bb1 ]
; CHECK-NEXT: [[FINALRET:%.*]] = tail call %swift.refcounted* @swift_retain(%swift.refcounted* [[PHIRET]])
; CHECK-NEXT: ret %swift.refcounted* [[FINALRET]]
define %swift.refcounted* @swift_contractRetainNInterleavedWithUnknown(%swift.refcounted* %A) {
entry:
  br i1 undef, label %bb1, label %bb2

bb1:
  tail call void @swift_retain_noresult(%swift.refcounted* %A)
  call void @user(%swift.refcounted* %A)
  tail call void @swift_retain_noresult(%swift.refcounted* %A)
  call void @noread_user(%swift.refcounted* %A)
  tail call void @swift_retain_noresult(%swift.refcounted* %A)
  call void @noread_user(%swift.refcounted* %A)
  tail call void @swift_retain_noresult(%swift.refcounted* %A)
  call void @user(%swift.refcounted* %A)  
  tail call void @swift_retain_noresult(%swift.refcounted* %A)
  tail call void @swift_retain_noresult(%swift.refcounted* %A)    
  call void @user(%swift.refcounted* %A)
  tail call void @swift_retain_noresult(%swift.refcounted* %A)  
  br label %bb3

bb2:
  call void @user(%swift.refcounted* %A)
  tail call void @swift_retain_noresult(%swift.refcounted* %A)
  call void @user(%swift.refcounted* %A)
  br label %bb3

bb3:
  tail call void @swift_retain_noresult(%swift.refcounted* %A)
  ret %swift.refcounted* %A
}

; CHECK-LABEL: define %swift.refcounted* @swift_contractReleaseNInterleavedWithUnknown(%swift.refcounted* %A) {
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

; CHECK-LABEL: define %swift.refcounted* @swift_contractRetainReleaseNInterleavedWithUnknown(%swift.refcounted* %A) {
; CHECK: bb1:
; CHECK-NEXT: [[RET1:%.*]] = tail call %swift.refcounted* @swift_retain(%swift.refcounted* %A)
; CHECK-NEXT: call void @user(%swift.refcounted* [[RET1]])
; CHECK-NEXT: call void @noread_user(%swift.refcounted* [[RET1]])
; CHECK-NEXT: tail call void @swift_release_n(%swift.refcounted* [[RET1]], i32 2)
; CHECK-NEXT: call void @user(%swift.refcounted* [[RET1]])
; CHECK-NEXT: [[RET2:%.*]] = tail call %swift.refcounted* @swift_retain_n(%swift.refcounted* [[RET1]], i32 2)
; CHECK-NEXT: tail call void @swift_release(%swift.refcounted* [[RET2]])
; CHECK-NEXT: call void @user(%swift.refcounted* [[RET2]])
; CHECK-NEXT: call void @noread_user(%swift.refcounted* [[RET2]])
; CHECK-NEXT: tail call void @swift_release_n(%swift.refcounted* [[RET2]], i32 2)
; CHECK-NEXT: call void @user(%swift.refcounted* [[RET2]])
; CHECK-NEXT: br label %bb3
; CHECK: bb2:
; CHECK-NEXT: call void @user(%swift.refcounted* %A)
; CHECK-NEXT: call void @swift_release(%swift.refcounted* %A)
; CHECK-NEXT: call void @user(%swift.refcounted* %A)
; CHECK-NEXT: br label %bb3

; CHECK: bb3:
; CHECK-NEXT: [[PHIRET:%.*]] = phi %swift.refcounted* [ %A, %bb2 ], [ [[RET2]], %bb1 ]
; CHECK-NEXT: tail call void @swift_release(%swift.refcounted* [[PHIRET]])
; CHECK-NEXT: ret %swift.refcounted* [[PHIRET]]
define %swift.refcounted* @swift_contractRetainReleaseNInterleavedWithUnknown(%swift.refcounted* %A) {
entry:
  br i1 undef, label %bb1, label %bb2

bb1:
  tail call void @swift_retain_noresult(%swift.refcounted* %A)  
  call void @user(%swift.refcounted* %A)
  tail call void @swift_release(%swift.refcounted* %A)
  call void @noread_user(%swift.refcounted* %A)
  tail call void @swift_release(%swift.refcounted* %A)
  call void @user(%swift.refcounted* %A)
  tail call void @swift_retain_noresult(%swift.refcounted* %A)
  tail call void @swift_retain_noresult(%swift.refcounted* %A)
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


!llvm.dbg.cu = !{!1}
!llvm.module.flags = !{!4}

!0 = !DILocation(line: 0, scope: !3)
!1 = distinct !DICompileUnit(language: DW_LANG_Swift, file: !2)
!2 = !DIFile(filename: "contract.swift", directory: "")
!3 = distinct !DISubprogram(name: "_", scope: !1, file: !2, type: !DISubroutineType(types: !{}))
!4 = !{i32 1, !"Debug Info Version", i32 3}

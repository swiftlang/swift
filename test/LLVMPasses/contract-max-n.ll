; Merged retain_n and release_n calls cover at most 256 calls each.

; RUN: %{python} %S/Inputs/gen-many-retains-releases.py 513 > %t.ll
; RUN: %swift-llvm-opt -passes=swift-llvm-arc-contract %t.ll | %FileCheck %s

; CHECK-LABEL: define void @many(ptr %A) {
; CHECK-NEXT: entry:
; CHECK-NEXT: call ptr @swift_retain_n(ptr %A, i32 256)
; CHECK-NEXT: call ptr @swift_retain_n(ptr %A, i32 256)
; CHECK-NEXT: call ptr @swift_retain(ptr %A)
; CHECK-NEXT: call void @user(ptr %A)
; CHECK-NEXT: call void @swift_release_n(ptr %A, i32 256)
; CHECK-NEXT: call void @swift_release_n(ptr %A, i32 256)
; CHECK-NEXT: call void @swift_release(ptr %A)
; CHECK-NEXT: ret void

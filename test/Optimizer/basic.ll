; RUN: %swift %s -arc-optimize | FileCheck %s
target datalayout = "e-p:64:64:64-S128-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f16:16:16-f32:32:32-f64:64:64-f128:128:128-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64"
target triple = "x86_64-apple-darwin11.3.0"

%swift.refcounted = type { %swift.heapmetadata*, i64 }
%swift.heapmetadata = type { i64 (%swift.refcounted*)*, i64 (%swift.refcounted*)* }

declare void @swift_release(%swift.refcounted* nocapture)
declare void @swift_retain_noresult(%swift.refcounted* nocapture) nounwind


define void @trivial_retain_release(%swift.refcounted* %P) {
entry:
  tail call void @swift_retain_noresult(%swift.refcounted* %P)
  tail call void @swift_release(%swift.refcounted* %P) nounwind
  ret void
}

; CHECK: @trivial_retain_release(
; CHECK-NEXT: entry:
; CHECK-NEXT: ret void

; RUN: %swift %s -arc-optimize | FileCheck %s
target datalayout = "e-p:64:64:64-S128-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f16:16:16-f32:32:32-f64:64:64-f128:128:128-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64"
target triple = "x86_64-apple-darwin11.3.0"

%swift.refcounted = type { %swift.heapmetadata*, i64 }
%swift.heapmetadata = type { i64 (%swift.refcounted*)*, i64 (%swift.refcounted*)* }

declare %swift.refcounted* @swift_allocObject(%swift.heapmetadata* , i64, i64) nounwind
declare void @swift_release(%swift.refcounted* nocapture)
declare %swift.refcounted* @swift_retain(%swift.refcounted* ) nounwind
declare void @swift_retain_noresult(%swift.refcounted* nocapture) nounwind
declare { i64, i64, i64 } @swift_retainAndReturnThree(%swift.refcounted* , i64, i64 , i64 )

define void @trivial_retain_release(%swift.refcounted* %P) {
entry:
  tail call void @swift_retain_noresult(%swift.refcounted* %P)
  tail call void @swift_release(%swift.refcounted* %P) nounwind
  ret void
}

; CHECK: @trivial_retain_release(
; CHECK-NEXT: entry:
; CHECK-NEXT: ret void


; rdar://11542743
define i64 @max_test(i64 %x) nounwind {
entry:
  %0 = tail call noalias %swift.refcounted* @swift_allocObject(%swift.heapmetadata* null, i64 24, i64 8) nounwind
  %1 = tail call %swift.refcounted* @swift_retain(%swift.refcounted* %0) nounwind
  tail call void @swift_release(%swift.refcounted* %0) nounwind
  %2 = icmp sgt i64 %x, 0
  %x.y.i = select i1 %2, i64 %x, i64 0
  %3 = tail call %swift.refcounted* @swift_retain(%swift.refcounted* %1) nounwind
  tail call void @swift_release(%swift.refcounted* %3) nounwind
  tail call void @swift_release(%swift.refcounted* %1) nounwind
  ret i64 %x.y.i
}
; CHECK: @max_test
; CHECK-NEXT: entry:
; CHECK-NEXT: icmp
; CHECK-NEXT: select
; CHECK-NEXT: ret


; retain3_test2 - This shows a case where something else (eg inlining an already
; optimized function) has given us a swift_retainAndReturnThree that we need to
; destructure and reassemble.
define { i8*, i64, %swift.refcounted* } @retain3_test2(i8*, i64, %swift.refcounted*) nounwind {
entry:
  %x = ptrtoint i8* %0 to i64
  %z = ptrtoint %swift.refcounted* %2 to i64
  
  %3 = call { i64, i64, i64 } @swift_retainAndReturnThree(%swift.refcounted* %2, i64 %x, i64 %1, i64 %z)
  %a = extractvalue { i64, i64, i64 } %3, 0
  %b = extractvalue { i64, i64, i64 } %3, 1
  %c = extractvalue { i64, i64, i64 } %3, 2
  %a1 = inttoptr i64 %a to i8*
  %c1 = inttoptr i64 %c to %swift.refcounted*
  %4 = insertvalue { i8*, i64, %swift.refcounted* } undef, i8* %a1, 0
  %5 = insertvalue { i8*, i64, %swift.refcounted* } %4, i64 %b, 1
  %6 = insertvalue { i8*, i64, %swift.refcounted* } %5, %swift.refcounted* %c1, 2
  ret { i8*, i64, %swift.refcounted* } %6
}

; CHECK: @retain3_test2
; CHECK: insertvalue
; CHECK-NEXT: insertvalue
; CHECK-NEXT: insertvalue
; CHECK-NEXT: call void @swift_retain_noresult(%swift.refcounted* %2)
; CHECK-NEXT: ret


; retain_motion1 - This shows motion of a retain across operations that can't
; release an object.  Release motion can't zap this.
define void @retain_motion1(%swift.refcounted* %A) {
  tail call void @swift_retain_noresult(%swift.refcounted* %A)
  %B = bitcast %swift.refcounted* %A to i32*
  store i32 42, i32* %B
  tail call void @swift_release(%swift.refcounted* %A) nounwind
  ret void
}

; CHECK: @retain_motion1(
; CHECK-NEXT: bitcast
; CHECK-NEXT: store i32
; CHECK-NEXT: ret void






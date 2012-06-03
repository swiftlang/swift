; RUN: %swift %s -arc-expand | FileCheck %s
target datalayout = "e-p:64:64:64-S128-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f16:16:16-f32:32:32-f64:64:64-f128:128:128-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64"
target triple = "x86_64-apple-darwin11.3.0"

%swift.refcounted = type { %swift.heapmetadata*, i64 }
%swift.heapmetadata = type { i64 (%swift.refcounted*)*, i64 (%swift.refcounted*)* }

declare %swift.refcounted* @swift_allocObject(%swift.heapmetadata* , i64, i64) nounwind
declare void @swift_release(%swift.refcounted* nocapture)
declare %swift.refcounted* @swift_retain(%swift.refcounted* ) nounwind
declare void @swift_retain_noresult(%swift.refcounted* nocapture) nounwind
declare { i64, i64, i64 } @swift_retainAndReturnThree(%swift.refcounted* , i64, i64 , i64 )

; rdar://11563395 - Synthesize calls to swift_retainAndReturnThree for better codegen
define { i8*, i64, %swift.refcounted* } @retain3_test1(i8*, i64, %swift.refcounted*) nounwind {
entry:
  %3 = bitcast i32 0 to i32
  call void @swift_retain_noresult(%swift.refcounted* %2)
  %4 = insertvalue { i8*, i64, %swift.refcounted* } undef, i8* %0, 0
  %5 = insertvalue { i8*, i64, %swift.refcounted* } %4, i64 %1, 1
  %6 = insertvalue { i8*, i64, %swift.refcounted* } %5, %swift.refcounted* %2, 2
  ret { i8*, i64, %swift.refcounted* } %6
}

; CHECK: @retain3_test1
; CHECK: tail call {{.*}} @swift_retainAndReturnThree
; CHECK: ret


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

; CHECK: @retain3_test2
; CHECK: tail call {{.*}} @swift_retainAndReturnThree
; CHECK: ret


define { i8*, i64, %swift.refcounted* } @retain3_test3(i8*, i64, %swift.refcounted** %this.owner) nounwind {
entry:
  %2 = load %swift.refcounted** %this.owner, align 8
  %3 = insertvalue { i8*, i64, %swift.refcounted* } undef, i8* %0, 0
  %4 = insertvalue { i8*, i64, %swift.refcounted* } %3, i64 %1, 1
  %5 = insertvalue { i8*, i64, %swift.refcounted* } %4, %swift.refcounted* %2, 2
  call void @swift_retain_noresult(%swift.refcounted* %2)
  ret { i8*, i64, %swift.refcounted* } %5
}

; CHECK: @retain3_test3
; CHECK: tail call {{.*}} @swift_retainAndReturnThree
; CHECK: ret


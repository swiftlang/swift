; RUN: %llvm-opt -swift-arc-optimize -S %s | FileCheck %s

target datalayout = "e-p:64:64:64-S128-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f16:16:16-f32:32:32-f64:64:64-f128:128:128-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64"
target triple = "x86_64-apple-macosx10.9"

%swift.refcounted = type { %swift.heapmetadata*, i64 }
%swift.heapmetadata = type { i64 (%swift.refcounted*)*, i64 (%swift.refcounted*)* }
%objc_object = type opaque

declare %objc_object* @objc_retain(%objc_object*)
declare void @objc_release(%objc_object*)
declare %swift.refcounted* @swift_allocObject(%swift.heapmetadata* , i64, i64) nounwind
declare void @swift_release(%swift.refcounted* nocapture)
declare %swift.refcounted* @swift_retain(%swift.refcounted* ) nounwind
declare void @swift_retain_noresult(%swift.refcounted* nocapture) nounwind
declare { i64, i64, i64 } @swift_retainAndReturnThree(%swift.refcounted* , i64, i64 , i64 )

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




; trivial_alloc_eliminate1 - Show that we can eliminate an allocation with a
; trivial destructor.
@trivial_dtor_metadata = internal constant %swift.heapmetadata { i64 (%swift.refcounted*)* @trivial_dtor, i64 (%swift.refcounted*)* null }
define internal i64 @trivial_dtor(%swift.refcounted* nocapture) nounwind readonly {
entry:
  %1 = getelementptr inbounds %swift.refcounted, %swift.refcounted* %0, i64 1
  %2 = bitcast %swift.refcounted* %1 to i64*
  %length = load i64, i64* %2, align 8
  %3 = shl i64 %length, 3
  %4 = add i64 %3, 24
  ret i64 %4
}
define void @trivial_alloc_eliminate1(i64 %x) nounwind {
entry:
  %0 = tail call noalias %swift.refcounted* @swift_allocObject(%swift.heapmetadata* @trivial_dtor_metadata, i64 24, i64 8) nounwind
  tail call void @swift_release(%swift.refcounted* %0) nounwind
  ret void
}
; CHECK: @trivial_alloc_eliminate1
; CHECK-NEXT: entry:
; CHECK-NEXT: ret void



; trivial_alloc_eliminate2 - Show that we can eliminate an allocation with a
; destructor that does a retain on the 'self' object.
@trivial_dtor_metadata2 = internal constant %swift.heapmetadata { i64 (%swift.refcounted*)* @trivial_dtor2, i64 (%swift.refcounted*)* null }
define internal i64 @trivial_dtor2(%swift.refcounted* nocapture %this) nounwind readonly {
entry:
  %0 = getelementptr inbounds %swift.refcounted, %swift.refcounted* %this, i64 1, i32 0
  store %swift.heapmetadata* inttoptr (i64 4 to %swift.heapmetadata*), %swift.heapmetadata** %0, align 8
  tail call %swift.refcounted* @swift_retain(%swift.refcounted* %this)
  ret i64 48
}
define void @trivial_alloc_eliminate2(i64 %x) nounwind {
entry:
  %0 = tail call noalias %swift.refcounted* @swift_allocObject(%swift.heapmetadata* @trivial_dtor_metadata2, i64 24, i64 8) nounwind
  tail call void @swift_release(%swift.refcounted* %0) nounwind
  ret void
}
; CHECK: @trivial_alloc_eliminate2
; CHECK-NEXT: entry:
; CHECK-NEXT: ret void




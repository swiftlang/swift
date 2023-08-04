; RUN: %swift-llvm-opt -passes=swift-llvm-arc-optimize %s | %FileCheck %s

target datalayout = "e-p:64:64:64-S128-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f16:16:16-f32:32:32-f64:64:64-f128:128:128-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64"
target triple = "x86_64-apple-macosx10.9"

%swift.refcounted = type { ptr, i64 }
%swift.heapmetadata = type { ptr, ptr }

declare ptr @swift_allocObject(ptr , i64, i64) nounwind
declare void @swift_release(ptr nocapture)
declare void @swift_retain(ptr ) nounwind
declare { i64, i64, i64 } @swift_retainAndReturnThree(ptr , i64, i64 , i64 )

; rdar://11542743
define i64 @max_test(i64 %x) nounwind {
entry:
  %0 = tail call noalias ptr @swift_allocObject(ptr null, i64 24, i64 8) nounwind
  tail call void @swift_retain(ptr %0) nounwind
  tail call void @swift_release(ptr %0) nounwind
  %1 = icmp sgt i64 %x, 0
  %x.y.i = select i1 %1, i64 %x, i64 0
  tail call void @swift_retain(ptr %0) nounwind
  tail call void @swift_release(ptr %0) nounwind
  tail call void @swift_release(ptr %0) nounwind
  ret i64 %x.y.i
}
; CHECK: @max_test
; CHECK-NEXT: entry:
; CHECK-NEXT: icmp
; CHECK-NEXT: select
; CHECK-NEXT: ret




; trivial_alloc_eliminate1 - Show that we can eliminate an allocation with a
; trivial destructor.
@trivial_dtor_metadata = internal constant %swift.heapmetadata { ptr @trivial_dtor, ptr null }
define internal i64 @trivial_dtor(ptr nocapture) nounwind readonly {
entry:
  %1 = getelementptr inbounds %swift.refcounted, ptr %0, i64 1
  %length = load i64, ptr %1, align 8
  %2 = shl i64 %length, 3
  %3 = add i64 %2, 24
  ret i64 %3
}
define void @trivial_alloc_eliminate1(i64 %x) nounwind {
entry:
  %0 = tail call noalias ptr @swift_allocObject(ptr @trivial_dtor_metadata, i64 24, i64 8) nounwind
  tail call void @swift_release(ptr %0) nounwind
  ret void
}
; CHECK: @trivial_alloc_eliminate1
; CHECK-NEXT: entry:
; CHECK-NEXT: ret void



; trivial_alloc_eliminate2 - Show that we can eliminate an allocation with a
; destructor that does a retain on the 'self' object.
@trivial_dtor_metadata2 = internal constant %swift.heapmetadata { ptr @trivial_dtor2, ptr null }
define internal i64 @trivial_dtor2(ptr nocapture %this) nounwind readonly {
entry:
  %0 = getelementptr inbounds %swift.refcounted, ptr %this, i64 1, i32 0
  store ptr inttoptr (i64 4 to ptr), ptr %0, align 8
  tail call void @swift_retain(ptr %this)
  ret i64 48
}
define void @trivial_alloc_eliminate2(i64 %x) nounwind {
entry:
  %0 = tail call noalias ptr @swift_allocObject(ptr @trivial_dtor_metadata2, i64 24, i64 8) nounwind
  tail call void @swift_release(ptr %0) nounwind
  ret void
}
; CHECK: @trivial_alloc_eliminate2
; CHECK-NEXT: entry:
; CHECK-NEXT: ret void


; external_dtor_alloc_eliminate -  Make sure we can not eliminate the allocation
; because we know nothing about the type of the allocation.
@external_dtor_metadata = external global %swift.heapmetadata, align 8
define void @external_dtor_alloc_eliminate() nounwind {
entry:
  %0 = tail call noalias ptr @swift_allocObject(ptr nonnull @external_dtor_metadata, i64 24, i64 8) nounwind
  tail call void @swift_release(ptr %0) nounwind
  ret void
}

; CHECK: @external_dtor_alloc_eliminate
; CHECK-NEXT: entry:
; CHECK-NEXT: swift_allocObject
; CHECK-NEXT: swift_release
; CHECK-NEXT: ret void


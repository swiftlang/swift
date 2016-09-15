; RUN: %swift-llvm-opt -swift-stack-promotion -stack-promotion-limit=100 %s | %FileCheck %s

target datalayout = "e-p:64:64:64-S128-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f16:16:16-f32:32:32-f64:64:64-f128:128:128-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64"
target triple = "x86_64-apple-macosx10.9"

%swift.type = type { i64 }
%objc_object = type opaque
%swift.refcounted = type { %swift.type*, i32, i32 }
%BufferStorageType = type <{ %swift.refcounted }>

; CHECK-LABEL: define{{( protected)?}} void @promote_buffer()
; CHECK: [[B:%.+]] = alloca i8, i32 48, align 8
; CHECK: [[M:%.+]] = call %swift.type* @get_buffer_metadata()
; CHECK: [[BC:%.+]] = bitcast i8* [[B]] to %objc_object*
; CHECK: [[I:%.+]] = call %objc_object* @swift_initStackObject(%swift.type* [[M]], %objc_object* [[BC]])
; CHECK: [[BC2:%.+]] = bitcast %objc_object* [[I]] to i8*
; CHECK: call void @llvm.lifetime.end(i64 -1, i8* [[BC2]])
; CHECK: ret void
define void @promote_buffer() {
entry:
  %0 = call %swift.type* @get_buffer_metadata()
  %1 = call %objc_object* @swift_bufferAllocateOnStack(%swift.type* %0, i64 48, i64 7)
  call void @swift_bufferDeallocateFromStack(%objc_object* %1)
  ret void
}

; CHECK-LABEL: define{{( protected)?}} void @promote_buffer_with_devirtualized_release()
; CHECK: [[B:%.+]] = alloca i8, i32 48, align 8
; CHECK: [[M:%.+]] = call %swift.type* @get_buffer_metadata()
; CHECK: [[BC:%.+]] = bitcast i8* [[B]] to %objc_object*
; CHECK: [[I:%.+]] = call %objc_object* @swift_initStackObject(%swift.type* [[M]], %objc_object* [[BC]])
; CHECK-NOT: call
; CHECK: [[BC2:%.+]] = bitcast %objc_object* [[I]] to i8*
; CHECK-NOT: call
; CHECK: call void @llvm.lifetime.end(i64 -1, i8* [[BC2]])
; CHECK-NOT: call
; CHECK: ret void
define void @promote_buffer_with_devirtualized_release() {
entry:
  %0 = call %swift.type* @get_buffer_metadata()
  %1 = call %objc_object* @swift_bufferAllocateOnStack(%swift.type* %0, i64 48, i64 7)
  %2 = bitcast %objc_object* %1 to %BufferStorageType*
  call void bitcast (void (%swift.refcounted*)* @swift_setDeallocating to void (%BufferStorageType*)*)(%BufferStorageType* %2)
  %3 = bitcast %BufferStorageType* %2 to %swift.refcounted*
  call void @swift_deallocClassInstance(%swift.refcounted* %3, i64 48, i64 7)
  call void @swift_bufferDeallocateFromStack(%objc_object* %1)
  ret void
}

; CHECK-LABEL: define{{( protected)?}} void @promote_buffer_with_devirtualized_release_and_non_trivial_deinit()
; CHECK: [[B:%.+]] = alloca i8, i32 48, align 8
; CHECK: [[M:%.+]] = call %swift.type* @get_buffer_metadata()
; CHECK: [[BC:%.+]] = bitcast i8* [[B]] to %objc_object*
; CHECK: [[I:%.+]] = call %objc_object* @swift_initStackObject(%swift.type* [[M]], %objc_object* [[BC]])
; CHECK: [[BC2:%.+]] = bitcast %objc_object* [[I]] to %BufferStorageType
; CHECK-NEXT: call {{.*}}@swift_setDeallocating {{.*}}({{.*}} [[BC2]])
; CHECK-NEXT: call void @unknown_deinit(%BufferStorageType* [[BC2]])
; CHECK-NOT: call
; CHECK: [[BC3:%.+]] = bitcast %objc_object* [[I]] to i8*
; CHECK-NEXT: call void @llvm.lifetime.end(i64 -1, i8* [[BC3]])
; CHECK-NOT: call
; CHECK: ret void
define void @promote_buffer_with_devirtualized_release_and_non_trivial_deinit() {
entry:
  %0 = call %swift.type* @get_buffer_metadata()
  %1 = call %objc_object* @swift_bufferAllocateOnStack(%swift.type* %0, i64 48, i64 7)
  %2 = bitcast %objc_object* %1 to %BufferStorageType*
  call void bitcast (void (%swift.refcounted*)* @swift_setDeallocating to void (%BufferStorageType*)*)(%BufferStorageType* %2)
  call void @unknown_deinit(%BufferStorageType* %2)
  %3 = bitcast %BufferStorageType* %2 to %swift.refcounted*
  call void @swift_deallocClassInstance(%swift.refcounted* %3, i64 48, i64 7)
  call void @swift_bufferDeallocateFromStack(%objc_object* %1)
  ret void
}

; CHECK-LABEL: define{{( protected)?}} void @dont_promote_buffer_exceeding_limit()
; CHECK: [[M:%.+]] = call %swift.type* @get_buffer_metadata()
; CHECK: call %objc_object* @swift_bufferAllocate(%swift.type* [[M]], i64 48, i64 7)
; CHECK-NEXT: ret void
define void @dont_promote_buffer_exceeding_limit() {
entry:
  %0 = alloca i8, i32 128, align 8
  %1 = call %swift.type* @get_buffer_metadata()
  %2 = call %objc_object* @swift_bufferAllocateOnStack(%swift.type* %1, i64 48, i64 7)
  call void @swift_bufferDeallocateFromStack(%objc_object* %2)
  ret void
}

declare %swift.type* @get_buffer_metadata()
declare %objc_object* @swift_bufferAllocateOnStack(%swift.type*, i64, i64)
declare void @swift_bufferDeallocateFromStack(%objc_object*)
declare void @swift_setDeallocating(%swift.refcounted*)
declare void @swift_deallocClassInstance(%swift.refcounted*, i64, i64)
declare void @unknown_deinit(%BufferStorageType*)

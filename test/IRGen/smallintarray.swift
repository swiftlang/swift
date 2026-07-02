// RUN: %target-swift-frontend -emit-ir -disable-availability-checking %s | %FileCheck %s

enum SmallIntArray<let count: Int> {
  case inline([count of Int], Int)
  case spilled([Int])
}

//===----------------------------------------------------------------------===//
// SmallIntArray<count> outlined destroy
//===----------------------------------------------------------------------===//

// CHECK-LABEL: define {{.*}} ptr @"$s13smallintarray13SmallIntArrayOyxGSiRVzlWOh"(ptr %0, ptr %"SmallIntArray<count>")
// CHECK:        [[TAG:%.*]] = call i32 @swift_getEnumCaseMultiPayload(ptr %0, ptr %"SmallIntArray<count>")
// CHECK-NEXT:   [[IS_INLINE:%.*]] = icmp ne i32 [[TAG]], 1
// CHECK-NEXT:   br i1 [[IS_INLINE]], label %[[EXIT:.*]], label %[[DESTROY_SPILLED:.*]]
// CHECK:      [[DESTROY_SPILLED]]:
// CHECK-NEXT:  [[BUFFER:%.*]] = getelementptr inbounds nuw %TSa, ptr %0, i32 0, i32 0
// CHECK-NEXT:  [[STORAGE:%.*]] = getelementptr inbounds nuw %Ts12_ArrayBufferV, ptr [[BUFFER]], i32 0, i32 0
// CHECK-NEXT:  [[RAW_VALUE:%.*]] = getelementptr inbounds nuw %Ts14_BridgeStorageV, ptr [[STORAGE]], i32 0, i32 0
// CHECK-NEXT:  [[TO_DESTROY:%.*]] = load ptr, ptr [[RAW_VALUE]], align 8
// CHECK-NEXT:  call void @swift_bridgeObjectRelease(ptr [[TO_DESTROY]])
// CHECK-NEXT:  br label %[[EXIT]]
// CHECK:      [[EXIT]]:
// CHECK-NEXT:   ret ptr %0

// RUN: %target-swift-frontend -emit-ir -disable-availability-checking %s | %FileCheck %s -check-prefix CHECK -check-prefix CHECK-%target-vendor
enum SmallIntArray<let count: Int> {
  case inline([count of Int], Int)
  case spilled([Int])
}

//===----------------------------------------------------------------------===//
// SmallIntArray<count> outlined destroy
//===----------------------------------------------------------------------===//

// CHECK-LABEL:        define {{.*}} ptr @"$s13smallintarray13SmallIntArrayOyxGSiRVzlWOh"(ptr %0, ptr %"SmallIntArray<count>")
// CHECK:                [[TAG:%.*]] = call i32 @swift_getEnumCaseMultiPayload(ptr %0, ptr %"SmallIntArray<count>")
// CHECK-NEXT:           [[IS_INLINE:%.*]] = icmp ne i32 [[TAG]], 1
// CHECK-NEXT:           br i1 [[IS_INLINE]], label %[[EXIT:.*]], label %[[DESTROY_SPILLED:.*]]
// CHECK:              [[DESTROY_SPILLED]]:
// CHECK-NEXT:           [[BUFFER:%.*]] = getelementptr inbounds nuw [[BUFFER_TYPE:%.*]], ptr %0, i32 0, i32 0
// CHECK-NEXT:           [[STORAGE:%.*]] = getelementptr inbounds nuw [[STORAGE_TYPE:%.*]], ptr [[BUFFER]], i32 0, i32 0
// CHECK-apple-NEXT:     [[RAW_VALUE:%.*]] = getelementptr inbounds nuw [[RAW_TYPE:%.*]], ptr [[STORAGE]], i32 0, i32 0
// CHECK-apple-NEXT:     [[TO_DESTROY:%.*]] = load ptr, ptr [[RAW_VALUE]]
// CHECK-unknown-NEXT:   [[TO_DESTROY:%.*]] = load ptr, ptr [[STORAGE]]
// CHECK-apple-NEXT:     call void @swift_bridgeObjectRelease(ptr [[TO_DESTROY]])
// CHECK-unknown-NEXT:   call void @swift_release(ptr [[TO_DESTROY]])
// CHECK-NEXT:           br label %[[EXIT]]
// CHECK:              [[EXIT]]:
// CHECK-NEXT:           ret ptr %0

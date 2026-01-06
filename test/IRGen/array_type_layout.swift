// RUN: %target-swift-frontend -emit-ir -disable-availability-checking %s | %FileCheck %s

struct VerySmallSlab<T> {
  var inline: InlineArray<16, T?>
  var count = 0

  init() {
    inline = .init(repeating: nil)
  }
}

//===----------------------------------------------------------------------===//
// VerySmallSlab<T> initializeBufferWithCopyOfBuffer
//===----------------------------------------------------------------------===//

// CHECK-LABEL: define {{.*}} ptr @"$s17array_type_layout13VerySmallSlabVwCP"(ptr{{.*}} %dest, ptr{{.*}} %src, ptr{{.*}} %"VerySmallSlab<T>")
// CHECK:         [[FLAGS:%.*]] = load i32, ptr {{.*}}
// CHECK-NEXT:    [[INLINE_BIT:%.*]] = and i32 [[FLAGS]], 131072
// CHECK-NEXT:    [[IS_INLINE:%.*]] = icmp eq i32 [[INLINE_BIT]], 0
// CHECK-NEXT:    br i1 [[IS_INLINE]], label %[[DIRECT_BB:.*]], label %[[INDIRECT_BB:.*]]
// CHECK-EMPTY:
// CHECK-NEXT:  [[INDIRECT_BB]]:
// CHECK:         br label %[[CONT_BB:.*]]
// CHECK-EMPTY:
// CHECK-NEXT:  [[DIRECT_BB]]:
// CHECK:         br label %[[LOOP_BB:.*]]
// CHECK-EMPTY:
// CHECK-NEXT:  [[LOOP_BB]]:
// CHECK-NEXT:    [[COUNT:%.*]] = phi i{{64|32}} [ 16, %[[DIRECT_BB]] ], [ [[SUB:%.*]], %[[PRED_BB:.*]] ]
// CHECK-NEXT:    [[DEST:%.*]] = phi ptr [ %dest, %[[DIRECT_BB]] ], [ [[DEST_OFFSET_PTR:%.*]], %[[PRED_BB]] ]
// CHECK-NEXT:    [[SRC:%.*]] = phi ptr [ %src, %[[DIRECT_BB]] ], [ [[SRC_OFFSET_PTR:%.*]], %[[PRED_BB]] ]
// CHECK:         [[TAG:%.*]] = call i32 %GetEnumTagSinglePayload(ptr{{.*}} [[SRC]], i32 1, ptr %T)
// CHECK-NEXT:    [[NIL_CHECK:%.*]] = icmp eq i32 [[TAG]], 0
// CHECK-NEXT:    br i1 [[NIL_CHECK]], label %[[NOT_NIL_BB:.*]], label %[[NIL_BB:.*]]
// CHECK-EMPTY:
// CHECK-NEXT:  [[NOT_NIL_BB]]:
// CHECK:         {{.*}} = call ptr %InitializeWithCopy(ptr{{.*}} [[DEST]], ptr{{.*}} [[SRC]], ptr %T)

// CHECK:       [[NIL_BB]]:
// CHECK:         call void @llvm.memcpy{{.*}}(ptr{{.*}} [[DEST]], ptr{{.*}} [[SRC]], i{{64|32}} {{%.*}}, i1 false)
// CHECK-NEXT:    br label %[[PRED_BB]]
// CHECK-EMPTY:
// CHECK-NEXT:  [[PRED_BB]]:
// CHECK:         [[SUB]] = sub i{{64|32}} [[COUNT]], 1
// CHECK-NEXT:    [[DONE:%.*]] = icmp eq i{{64|32}} [[SUB]], 0
// CHECK-NEXT:    br i1 [[DONE]], label %[[END_LOOP_BB:.*]], label %[[LOOP_BB]]
// CHECK-EMPTY:
// CHECK-NEXT:  [[END_LOOP_BB]]:
// CHECK:         [[DEST_COUNT_PTR:%.*]] = getelementptr inbounds i8, ptr %dest, i32 {{%.*}}
// CHECK:         [[SRC_COUNT_PTR:%.*]] = getelementptr inbounds i8, ptr %src, i32 {{%.*}}
// CHECK-NEXT:    call void @llvm.memcpy{{.*}}(ptr{{.*}} [[DEST_COUNT_PTR]], ptr{{.*}} [[SRC_COUNT_PTR]], i{{64 8|32 4}}, i1 false)
// CHECK-NEXT:    br label %[[CONT_BB]]
// CHECK-EMPTY:
// CHECK-NEXT:  [[CONT_BB]]:
// CHECK-NEXT:    [[RETURN_DEST_PTR:%.*]] = phi ptr [ {{%.*}}, %[[INDIRECT_BB]] ], [ %dest, %[[END_LOOP_BB]] ]
// CHECK-NEXT:    ret ptr [[RETURN_DEST_PTR]]

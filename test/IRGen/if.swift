// RUN: %swift -triple x86_64-apple-darwin10 %s -emit-llvm | FileCheck %s

import swift

// CHECK: [[INT:%.*]] = type { { i64 } }

// CHECK:    define i64 @_T2f0(i64) {
func f0(x : int) -> int {
  // Prologue.
  // CHECK:      [[RET:%.*]] = alloca [[INT]], align 8
  // CHECK-NEXT: [[X:%.*]] = alloca [[INT]], align 8
  // CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds [[INT]]* [[X]], i32 0, i32 0
  // CHECK-NEXT: [[T1:%.*]] = getelementptr inbounds { i64 }* [[T0]], i32 0, i32 0
  // CHECK-NEXT: store i64 {{%.*}}, i64* [[T1]], align 8

  // CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds [[INT]]* [[X]], i32 0, i32 0
  // CHECK-NEXT: [[T1:%.*]] = getelementptr inbounds { i64 }* [[T0]], i32 0, i32 0
  // CHECK-NEXT: [[T2:%.*]] = load i64* [[T1]], align 8
  // CHECK-NEXT: [[T3:%.*]] = call i1 @_Top2ee(i64 [[T2]], i64 0)
  // CHECK-NEXT: [[T4:%.*]] = call i1 @_T19convertToLogicValue(i1 [[T3]])
  // CHECK-NEXT: br i1 [[T4]], label %[[BB0:.*]], label %[[BB1:.*]]
  if (x == 0) {
    // CHECK:    [[BB0]]:
    // CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds [[INT]]* [[RET]], i32 0, i32 0
    // CHECK-NEXT: [[T1:%.*]] = getelementptr inbounds { i64 }* [[T0]], i32 0, i32 0
    // CHECK-NEXT: store i64 0, i64* [[T1]], align 8
    // CHECK-NEXT: br label %[[BB_RET:.*]]
    return 0

  // CHECK:    [[BB1]]:
  // CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds [[INT]]* [[X]], i32 0, i32 0
  // CHECK-NEXT: [[T1:%.*]] = getelementptr inbounds { i64 }* [[T0]], i32 0, i32 0
  // CHECK-NEXT: [[T2:%.*]] = load i64* [[T1]], align 8
  // CHECK-NEXT: [[T3:%.*]] = call i1 @_Top2ee(i64 [[T2]], i64 1)
  // CHECK-NEXT: [[T4:%.*]] = call i1 @_T19convertToLogicValue(i1 [[T3]])
  // CHECK-NEXT: br i1 [[T4]], label %[[BB2:.*]], label %[[BB3:.*]]
  } else if (x == 1) {
    // CHECK:    [[BB2]]:
    // CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds [[INT]]* [[RET]], i32 0, i32 0
    // CHECK-NEXT: [[T1:%.*]] = getelementptr inbounds { i64 }* [[T0]], i32 0, i32 0
    // CHECK-NEXT: store i64 1, i64* [[T1]], align 8
    // CHECK-NEXT: br label %[[BB_RET]]
    return 1

  } else {
    // CHECK:    [[BB3]]:
    // CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds [[INT]]* [[X]], i32 0, i32 0
    // CHECK-NEXT: [[T1:%.*]] = getelementptr inbounds { i64 }* [[T0]], i32 0, i32 0
    // CHECK-NEXT: [[T2:%.*]] = load i64* [[T1]], align 8
    // CHECK-NEXT: [[T3:%.*]] = call i64 @_Top1s(i64 [[T2]], i64 2)
    // CHECK-NEXT: [[V0:%.*]] = call i64 @_T2f0(i64 [[T3]])
    // CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds [[INT]]* [[X]], i32 0, i32 0
    // CHECK-NEXT: [[T1:%.*]] = getelementptr inbounds { i64 }* [[T0]], i32 0, i32 0
    // CHECK-NEXT: [[T2:%.*]] = load i64* [[T1]], align 8
    // CHECK-NEXT: [[T3:%.*]] = call i64 @_Top1s(i64 [[T2]], i64 1)
    // CHECK-NEXT: [[V1:%.*]] = call i64 @_T2f0(i64 [[T3]])
    // CHECK-NEXT: [[T0:%.*]] = call i64 @_Top1p(i64 [[V0]], i64 [[V1]])
    // CHECK-NEXT: [[T1:%.*]] = getelementptr inbounds [[INT]]* [[RET]], i32 0, i32 0
    // CHECK-NEXT: [[T2:%.*]] = getelementptr inbounds { i64 }* [[T1]], i32 0, i32 0
    // CHECK-NEXT: store i64 [[T0]], i64* [[T2]], align 8
    // CHECK-NEXT: br label %[[BB_RET]]
    return f0(x - 2) + f0(x - 1)
  }

  // CHECK:    [[BB_RET]]:
  // CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds [[INT]]* [[RET]], i32 0, i32 0
  // CHECK-NEXT: [[T1:%.*]] = getelementptr inbounds { i64 }* [[T0]], i32 0, i32 0
  // CHECK-NEXT: [[T2:%.*]] = load i64* [[T1]], align 8
  // CHECK-NEXT: ret i64 [[T2]]
}

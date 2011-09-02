// RUN: %swift -triple x86_64-apple-darwin10 %s -emit-llvm | FileCheck %s

import swift

// CHECK: [[INT:%.*]] = type { { i64 } }

func f0(x : int) -> int {
  x = x + 1
  return x
}
// CHECK:    define i64 @_T2f0(i64) {
// CHECK:      [[RET:%.*]] = alloca [[INT]], align 8
// CHECK-NEXT: [[X:%.*]] = alloca [[INT]], align 8
// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds [[INT]]* [[X]], i32 0, i32 0
// CHECK-NEXT: [[T1:%.*]] = getelementptr inbounds { i64 }* [[T0]], i32 0, i32 0
// CHECK-NEXT: store i64 {{%.*}}, i64* [[T1]], align 8

// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds [[INT]]* [[X]], i32 0, i32 0
// CHECK-NEXT: [[T1:%.*]] = getelementptr inbounds { i64 }* [[T0]], i32 0, i32 0
// CHECK-NEXT: [[T2:%.*]] = load i64* [[T1]], align 8
// CHECK-NEXT: [[T3:%.*]] = call i64 @_Top1p(i64 [[T2]], i64 1)
// CHECK-NEXT: [[T4:%.*]] = getelementptr inbounds [[INT]]* [[X]], i32 0, i32 0
// CHECK-NEXT: [[T5:%.*]] = getelementptr inbounds { i64 }* [[T4]], i32 0, i32 0
// CHECK-NEXT: store i64 [[T3]], i64* [[T5]], align 8

// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds [[INT]]* [[X]], i32 0, i32 0
// CHECK-NEXT: [[T1:%.*]] = getelementptr inbounds { i64 }* [[T0]], i32 0, i32 0
// CHECK-NEXT: [[T2:%.*]] = load i64* [[T1]], align 8
// CHECK-NEXT: [[T3:%.*]] = getelementptr inbounds [[INT]]* [[RET]], i32 0, i32 0
// CHECK-NEXT: [[T4:%.*]] = getelementptr inbounds { i64 }* [[T3]], i32 0, i32 0
// CHECK-NEXT: store i64 [[T2]], i64* [[T4]], align 8

// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds [[INT]]* [[RET]], i32 0, i32 0
// CHECK-NEXT: [[T1:%.*]] = getelementptr inbounds { i64 }* [[T0]], i32 0, i32 0
// CHECK-NEXT: [[T2:%.*]] = load i64* [[T1]], align 8
// CHECK-NEXT: ret i64 [[T2]]

// RUN: %swift -triple x86_64-apple-darwin10 -I %S/.. %s -emit-llvm | FileCheck %s

import swift

// CHECK: [[INT:%.*]] = type { { i64 } }

func f0(point : (int, int), z : int) -> int {
  return point.$0 + point.$1 + z
}

// CHECK:    define i64 @_T6tuples2f0FT5pointTNSs5int64S0__1zS0__S0_(
// CHECK:      [[RET:%.*]] = alloca [[INT]], align 8
// CHECK-NEXT: [[POINT:%.*]] = alloca { [[INT]], [[INT]] }, align 8
// CHECK-NEXT: [[Z:%.*]] = alloca [[INT]], align 8
// CHECK:      getelementptr inbounds { [[INT]], [[INT]] }* [[POINT]], i32 0, i32 0
// CHECK:      getelementptr inbounds { [[INT]], [[INT]] }* [[POINT]], i32 0, i32 1
// CHECK:      [[T0:%.*]] = getelementptr inbounds { [[INT]], [[INT]] }* [[POINT]], i32 0, i32 0
// CHECK-NEXT: [[T1:%.*]] = getelementptr inbounds [[INT]]* [[T0]], i32 0, i32 0
// CHECK-NEXT: [[T2:%.*]] = getelementptr inbounds { i64 }* [[T1]], i32 0, i32 0
// CHECK-NEXT: [[V0:%.*]] = load i64* [[T2]], align 8
// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds { [[INT]], [[INT]] }* [[POINT]], i32 0, i32 1
// CHECK-NEXT: [[T1:%.*]] = getelementptr inbounds [[INT]]* [[T0]], i32 0, i32 0
// CHECK-NEXT: [[T2:%.*]] = getelementptr inbounds { i64 }* [[T1]], i32 0, i32 0
// CHECK-NEXT: [[V1:%.*]] = load i64* [[T2]], align 8
// CHECK-NEXT: [[V2:%.*]] = call i64 @_TSsop1pFT3lhsNSs5int643rhsS__S_(i64 [[V0]], i64 [[V1]])
// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds [[INT]]* [[Z]], i32 0, i32 0
// CHECK-NEXT: [[T1:%.*]] = getelementptr inbounds { i64 }* [[T0]], i32 0, i32 0
// CHECK-NEXT: [[V3:%.*]] = load i64* [[T1]], align 8
// CHECK-NEXT: [[V4:%.*]] = call i64 @_TSsop1pFT3lhsNSs5int643rhsS__S_(i64 [[V2]], i64 [[V3]])
// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds [[INT]]* [[RET]], i32 0, i32 0
// CHECK-NEXT: [[T1:%.*]] = getelementptr inbounds { i64 }* [[T0]], i32 0, i32 0
// CHECK-NEXT: store i64 [[V4]], i64* [[T1]]

func f1(point : (int, int, int, int)) -> int {
  return point.$1 + point.$3
}
// CHECK:    define i64 @_T6tuples2f1FT5pointTNSs5int64S0_S0_S0___S0_(
// CHECK:      [[RET:%.*]] = alloca [[INT]], align 8
// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds { [[INT]], [[INT]], [[INT]], [[INT]] }* [[POINT:%.*]], i32 0, i32 1
// CHECK-NEXT: [[T1:%.*]] = getelementptr inbounds [[INT]]* [[T0]], i32 0, i32 0
// CHECK-NEXT: [[T2:%.*]] = getelementptr inbounds { i64 }* [[T1]], i32 0, i32 0
// CHECK-NEXT: [[V0:%.*]] = load i64* [[T2]], align 8
// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds { [[INT]], [[INT]], [[INT]], [[INT]] }* [[POINT]], i32 0, i32 3
// CHECK-NEXT: [[T1:%.*]] = getelementptr inbounds [[INT]]* [[T0]], i32 0, i32 0
// CHECK-NEXT: [[T2:%.*]] = getelementptr inbounds { i64 }* [[T1]], i32 0, i32 0
// CHECK-NEXT: [[V1:%.*]] = load i64* [[T2]], align 8
// CHECK-NEXT: [[V2:%.*]] = call i64 @_TSsop1pFT3lhsNSs5int643rhsS__S_(i64 [[V0]], i64 [[V1]])

func f2_helper() -> (int, int, int, int)
func f2() -> int {
  return f2_helper().$2
}
// CHECK:    define i64 @_T6tuples2f2FT_NSs5int64(
// CHECK:      [[RET:%.*]] = alloca [[INT]], align 8
// CHECK-NEXT: [[RESULT:%.*]] = alloca { [[INT]], [[INT]], [[INT]], [[INT]] }, align 8
// CHECK-NEXT: call void @_T6tuples9f2_helperFT_TNSs5int64S0_S0_S0__({ [[INT]], [[INT]], [[INT]], [[INT]] }* noalias sret [[RESULT]])
// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds { [[INT]], [[INT]], [[INT]], [[INT]] }* [[RESULT]], i32 0, i32 2
// CHECK-NEXT: [[T1:%.*]] = getelementptr inbounds [[INT]]* [[T0]], i32 0, i32 0
// CHECK-NEXT: [[T2:%.*]] = getelementptr inbounds { i64 }* [[T1]], i32 0, i32 0
// CHECK-NEXT: [[V0:%.*]] = load i64* [[T2]], align 8
// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds [[INT]]* [[RET]], i32 0, i32 0
// CHECK-NEXT: [[T1:%.*]] = getelementptr inbounds { i64 }* [[T0]], i32 0, i32 0
// CHECK-NEXT: store i64 [[V0]], i64* [[T1]]

func f3_helper() -> (int, int, int)
func f3() -> int {
  return f3_helper().$2
}
// CHECK:    define i64 @_T6tuples2f3FT_NSs5int64(
// CHECK:      [[RET:%.*]] = alloca [[INT]], align 8
// CHECK-NEXT: [[T0:%.*]] = call { i64, i64, i64 } @_T6tuples9f3_helperFT_TNSs5int64S0_S0__()
// CHECK-NEXT: [[V0:%.*]] = extractvalue { i64, i64, i64 } [[T0]], 0
// CHECK-NEXT: [[V1:%.*]] = extractvalue { i64, i64, i64 } [[T0]], 1
// CHECK-NEXT: [[V2:%.*]] = extractvalue { i64, i64, i64 } [[T0]], 2
// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds [[INT]]* [[RET]], i32 0, i32 0
// CHECK-NEXT: [[T1:%.*]] = getelementptr inbounds { i64 }* [[T0]], i32 0, i32 0
// CHECK-NEXT: store i64 [[V2]], i64* [[T1]]

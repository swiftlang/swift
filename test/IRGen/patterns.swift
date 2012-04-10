// RUN: %swift -triple x86_64-apple-darwin10 -I %S/.. %s -emit-llvm | FileCheck %s

// CHECK: [[INT:%.*]] = type { { i64 } }

import swift

func f0_helper() -> int
func f0() {
  var x : int = f0_helper()
}
// CHECK:    define void @_T8patterns2f0FT_T_(
// CHECK:      [[X:%.*]] = alloca [[INT]], align 8
// CHECK-NEXT: [[V0:%.*]] = call i64 @_T8patterns9f0_helperFT_NSs5int64()
// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds [[INT]]* [[X]], i32 0, i32 0
// CHECK-NEXT: [[T1:%.*]] = getelementptr inbounds { i64 }* [[T0]], i32 0, i32 0
// CHECK-NEXT: store i64 [[V0]], i64* [[T1]], align 8
// CHECK-NEXT: ret void

func f1_helper() -> (int, int)
func f1() {
  var (x, y) = f1_helper()
}
// CHECK:    define void @_T8patterns2f1FT_T_(
// CHECK:      [[X:%.*]] = alloca [[INT]], align 8
// CHECK-NEXT: [[Y:%.*]] = alloca [[INT]], align 8
// CHECK-NEXT: [[T0:%.*]] = call { i64, i64 } @_T8patterns9f1_helperFT_TNSs5int64S0__()
// CHECK-NEXT: [[V0:%.*]] = extractvalue { i64, i64 } [[T0]], 0
// CHECK-NEXT: [[V1:%.*]] = extractvalue { i64, i64 } [[T0]], 1
// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds [[INT]]* [[X]], i32 0, i32 0
// CHECK-NEXT: [[T1:%.*]] = getelementptr inbounds { i64 }* [[T0]], i32 0, i32 0
// CHECK-NEXT: store i64 [[V0]], i64* [[T1]], align 8
// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds [[INT]]* [[Y]], i32 0, i32 0
// CHECK-NEXT: [[T1:%.*]] = getelementptr inbounds { i64 }* [[T0]], i32 0, i32 0
// CHECK-NEXT: store i64 [[V1]], i64* [[T1]], align 8
// CHECK-NEXT: ret void

func f2() {
  var (x0, y0) = (0, 0)
  var (x1, y1) = (x0, y0)
}
// CHECK:    define void @_T8patterns2f2FT_T_(
// CHECK:      [[X0:%.*]] = alloca [[INT]], align 8
// CHECK-NEXT: [[Y0:%.*]] = alloca [[INT]], align 8
// CHECK-NEXT: [[X1:%.*]] = alloca [[INT]], align 8
// CHECK-NEXT: [[Y1:%.*]] = alloca [[INT]], align 8
// CHECK-NEXT: [[V0:%.*]] = call i64 @_TNSs5int6425convertFromIntegerLiteralFT3vali64_S_(i64 0)
// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds [[INT]]* [[X0]], i32 0, i32 0
// CHECK-NEXT: [[T1:%.*]] = getelementptr inbounds { i64 }* [[T0]], i32 0, i32 0
// CHECK-NEXT: store i64 [[V0]], i64* [[T1]], align 8
// CHECK-NEXT: [[V1:%.*]] = call i64 @_TNSs5int6425convertFromIntegerLiteralFT3vali64_S_(i64 0)
// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds [[INT]]* [[Y0]], i32 0, i32 0
// CHECK-NEXT: [[T1:%.*]] = getelementptr inbounds { i64 }* [[T0]], i32 0, i32 0
// CHECK-NEXT: store i64 [[V1]], i64* [[T1]], align 8
// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds [[INT]]* [[X0]], i32 0, i32 0
// CHECK-NEXT: [[T1:%.*]] = getelementptr inbounds { i64 }* [[T0]], i32 0, i32 0
// CHECK-NEXT: [[V0:%.*]] = load i64* [[T1]], align 8
// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds [[INT]]* [[X1]], i32 0, i32 0
// CHECK-NEXT: [[T1:%.*]] = getelementptr inbounds { i64 }* [[T0]], i32 0, i32 0
// CHECK-NEXT: store i64 [[V0]], i64* [[T1]], align 8
// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds [[INT]]* [[Y0]], i32 0, i32 0
// CHECK-NEXT: [[T1:%.*]] = getelementptr inbounds { i64 }* [[T0]], i32 0, i32 0
// CHECK-NEXT: [[V0:%.*]] = load i64* [[T1]], align 8
// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds [[INT]]* [[Y1]], i32 0, i32 0
// CHECK-NEXT: [[T1:%.*]] = getelementptr inbounds { i64 }* [[T0]], i32 0, i32 0
// CHECK-NEXT: store i64 [[V0]], i64* [[T1]], align 8
// CHECK-NEXT: ret void


func f3() {
  var p0 = (0, 0)
  var (x1, _) = p0
}
// CHECK:    define void @_T8patterns2f3FT_T_(
// CHECK:      [[P0:%.*]] = alloca { [[INT]], [[INT]] }, align 8
// CHECK-NEXT: [[X1:%.*]] = alloca [[INT]], align 8
// CHECK-NEXT: [[V0:%.*]] = call i64 @_TNSs5int6425convertFromIntegerLiteralFT3vali64_S_(
// CHECK-NEXT: [[V1:%.*]] = call i64 @_TNSs5int6425convertFromIntegerLiteralFT3vali64_S_(
// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds { [[INT]], [[INT]] }* [[P0]], i32 0, i32 0
// CHECK-NEXT: [[T1:%.*]] = getelementptr inbounds [[INT]]* [[T0]], i32 0, i32 0
// CHECK-NEXT: [[T2:%.*]] = getelementptr inbounds { i64 }* [[T1]], i32 0, i32 0
// CHECK-NEXT: store i64 [[V0]], i64* [[T2]], align 8
// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds { [[INT]], [[INT]] }* [[P0]], i32 0, i32 1
// CHECK-NEXT: [[T1:%.*]] = getelementptr inbounds [[INT]]* [[T0]], i32 0, i32 0
// CHECK-NEXT: [[T2:%.*]] = getelementptr inbounds { i64 }* [[T1]], i32 0, i32 0
// CHECK-NEXT: store i64 [[V1]], i64* [[T2]], align 8
// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds { [[INT]], [[INT]] }* [[P0]], i32 0, i32 0
// CHECK-NEXT: [[T1:%.*]] = getelementptr inbounds [[INT]]* [[T0]], i32 0, i32 0
// CHECK-NEXT: [[T2:%.*]] = getelementptr inbounds { i64 }* [[T1]], i32 0, i32 0
// CHECK-NEXT: [[V0:%.*]] = load i64* [[T2]], align 8
// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds [[INT]]* [[X1]], i32 0, i32 0
// CHECK-NEXT: [[T1:%.*]] = getelementptr inbounds { i64 }* [[T0]], i32 0, i32 0
// CHECK-NEXT: store i64 [[V0]], i64* [[T1]], align 8
// CHECK-NEXT: ret void

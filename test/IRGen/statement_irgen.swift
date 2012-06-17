// RUN: %swift -triple x86_64-apple-darwin10 %s -emit-llvm | FileCheck %s

// CHECK: [[INT:%.*]] = type { i64 }

// CHECK:    define i64 @_T2if2f0FT1xNSs5Int64_S0_(i64 %x) {
func f0(x : Int) -> Int {
  // Prologue.
  // CHECK:      [[RET:%.*]] = alloca [[INT]], align 8
  // CHECK-NEXT: [[X:%.*]] = alloca [[INT]], align 8
  // CHECK-NEXT: [[TEMP1:%.*]] = alloca %_TSs4Bool, align 1
  // CHECK-NEXT: [[TEMP2:%.*]] = alloca %_TSs4Bool, align 1
  // CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds [[INT]]* [[X]], i32 0, i32 0
  // CHECK-NEXT: store i64 {{%.*}}, i64* [[T0]], align 8

  // CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds [[INT]]* [[X]], i32 0, i32 0
  // CHECK-NEXT: [[T1:%.*]] = load i64* [[T0]], align 8
  // CHECK-NEXT: [[C0:%.*]] = call i64 @_TNSs5Int6425convertFromIntegerLiteralFT3vali64_S_(i64 0)
  // CHECK-NEXT: [[T2:%.*]] = call i1 @_TSsop2eeFT3lhsNSs5Int643rhsS__NSs4Bool(i64 [[T1]], i64 [[C0]])
  // CHECK-NEXT: [[T3:%.*]] = getelementptr inbounds %_TSs4Bool* [[TEMP1]], i32 0, i32 0
  // CHECK-NEXT: store i1 [[T2]], i1* [[T3]], align 1
  // CHECK-NEXT: [[T4:%.*]] = getelementptr inbounds %_TSs4Bool* [[TEMP1]], i32 0, i32 0
  // CHECK-NEXT: [[T5:%.*]] = load i1* [[T4]]
  // CHECK-NEXT: br i1 [[T5]], label %[[BB0:.*]], label %[[BB1:.*]]
  if (x == 0) {
    // CHECK:    [[BB0]]:
    // CHECK-NEXT: [[C0:%.*]] = call i64 @_TNSs5Int6425convertFromIntegerLiteralFT3vali64_S_(i64 0)
    // CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds [[INT]]* [[RET]], i32 0, i32 0
    // CHECK-NEXT: store i64 [[C0]], i64* [[T0]], align 8
    // CHECK-NEXT: br label %[[BB_RET:.*]]
    return 0

  // CHECK:    [[BB1]]:
  // CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds [[INT]]* [[X]], i32 0, i32 0
  // CHECK-NEXT: [[T1:%.*]] = load i64* [[T0]], align 8
  // CHECK-NEXT: [[C0:%.*]] = call i64 @_TNSs5Int6425convertFromIntegerLiteralFT3vali64_S_(i64 1)
  // CHECK-NEXT: [[T2:%.*]] = call i1 @_TSsop2eeFT3lhsNSs5Int643rhsS__NSs4Bool(i64 [[T1]], i64 [[C0]])
  // CHECK-NEXT: [[T3:%.*]] = getelementptr inbounds %_TSs4Bool* [[TEMP2]], i32 0, i32 0
  // CHECK-NEXT: store i1 [[T2]], i1* [[T3]], align 1
  // CHECK-NEXT: [[T4:%.*]] = getelementptr inbounds %_TSs4Bool* [[TEMP2]], i32 0, i32 0
  // CHECK-NEXT: [[T5:%.*]] = load i1* [[T4]]
  // CHECK-NEXT: br i1 [[T5]], label %[[BB2:.*]], label %[[BB3:.*]]
  } else if (x == 1) {
    // CHECK:    [[BB2]]:
    // CHECK-NEXT: [[C0:%.*]] = call i64 @_TNSs5Int6425convertFromIntegerLiteralFT3vali64_S_(i64 1)
    // CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds [[INT]]* [[RET]], i32 0, i32 0
    // CHECK-NEXT: store i64 [[C0]], i64* [[T0]], align 8
    // CHECK-NEXT: br label %[[BB_RET]]
    return 1

  } else {
    // CHECK:    [[BB3]]:
    // CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds [[INT]]* [[X]], i32 0, i32 0
    // CHECK-NEXT: [[T1:%.*]] = load i64* [[T0]], align 8
    // CHECK-NEXT: [[C0:%.*]] = call i64 @_TNSs5Int6425convertFromIntegerLiteralFT3vali64_S_(i64 2)
    // CHECK-NEXT: [[T2:%.*]] = call i64 @_TSsop1sFT3lhsNSs5Int643rhsS__S_(i64 [[T1]], i64 [[C0]])
    // CHECK-NEXT: [[V0:%.*]] = call i64 @_T2if2f0FT1xNSs5Int64_S0_(i64 [[T2]])
    // CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds [[INT]]* [[X]], i32 0, i32 0
    // CHECK-NEXT: [[T1:%.*]] = load i64* [[T0]], align 8
    // CHECK-NEXT: [[C0:%.*]] = call i64 @_TNSs5Int6425convertFromIntegerLiteralFT3vali64_S_(i64 1)
    // CHECK-NEXT: [[T2:%.*]] = call i64 @_TSsop1sFT3lhsNSs5Int643rhsS__S_(i64 [[T1]], i64 [[C0]])
    // CHECK-NEXT: [[V1:%.*]] = call i64 @_T2if2f0FT1xNSs5Int64_S0_(i64 [[T2]])
    // CHECK-NEXT: [[T0:%.*]] = call i64 @_TSsop1pFT3lhsNSs5Int643rhsS__S_(i64 [[V0]], i64 [[V1]])
    // CHECK-NEXT: [[T1:%.*]] = getelementptr inbounds [[INT]]* [[RET]], i32 0, i32 0
    // CHECK-NEXT: store i64 [[T0]], i64* [[T1]], align 8
    // CHECK-NEXT: br label %[[BB_RET]]
    return f0(x - 2) + f0(x - 1)
  }

  // CHECK:    [[BB_RET]]:
  // CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds [[INT]]* [[RET]], i32 0, i32 0
  // CHECK-NEXT: [[T1:%.*]] = load i64* [[T0]], align 8
  // CHECK-NEXT: ret i64 [[T1]]
}

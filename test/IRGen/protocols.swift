// RUN: %swift -triple x86_64-apple-darwin10 %s -emit-llvm | FileCheck %s

// CHECK: [[SET:%_T9protocols3Set]] = type { i8**, [[BUFFER:.*]] }

protocol Set {
  func empty() -> Bool
  func size() -> Int
}

func test0_helper() -> Set
func test0() {
  var s = test0_helper()
}
// CHECK:    define void @_T9protocols5test0FT_T_()
// CHECK:      [[S:%.*]] = alloca [[SET]], align 8
// CHECK-NEXT: call void @_T9protocols12test0_helperFT_PS_3Set([[SET]]* noalias sret [[S]])
// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds [[SET]]* [[S]], i32 0, i32 0
// CHECK-NEXT: [[WT:%.*]] = load i8*** [[T0]], align 8
// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds [[SET]]* [[S]], i32 0, i32 1
// CHECK-NEXT: [[T1:%.*]] = load i8** [[WT]], align 8
// CHECK-NEXT: [[FN:%.*]] = bitcast i8* [[T1]] to void ([[BUFFER]]*, i8**)*
// CHECK-NEXT: call void [[FN]]([[BUFFER]]* [[T0]], i8** [[WT]]) nounwind
// CHECK-NEXT: ret void

func test1(arg : Set) -> Set {
  var local = arg
  return local
}
// CHECK:    define void @_T9protocols5test1FT3argPS_3Set_S0_([[SET]]*, [[SET]]* [[ARG:%arg]]) {
// CHECK:      [[LOCAL:%.*]] = alloca [[SET]], align 8

// Copy arg directly into local.
// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds [[SET]]* [[ARG]], i32 0, i32 0
// CHECK-NEXT: [[WT:%.*]] = load i8*** [[T0]], align 8
// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds [[SET]]* [[LOCAL]], i32 0, i32 0
// CHECK-NEXT: store i8** [[WT]], i8*** [[T0]], align 8
// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds [[SET]]* [[ARG]], i32 0, i32 1
// CHECK-NEXT: [[T1:%.*]] = getelementptr inbounds [[SET]]* [[LOCAL]], i32 0, i32 1
// CHECK-NEXT: [[T2:%.*]] = getelementptr inbounds i8** [[WT]], i32 1
// CHECK-NEXT: [[T3:%.*]] = load i8** [[T2]], align 8
// CHECK-NEXT: [[FN:%.*]] = bitcast i8* [[T3]] to i8* ([[BUFFER]]*, [[BUFFER]]*, i8**)*
// CHECK-NEXT: call i8* [[FN]]([[BUFFER]]* noalias [[T1]], [[BUFFER]]* [[T0]], i8** [[WT]])

// Copy local directly into the result.
// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds [[SET]]* [[LOCAL]], i32 0, i32 0
// CHECK-NEXT: [[WT:%.*]] = load i8*** [[T0]], align 8
// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds [[SET]]* [[RESULT:%0]], i32 0, i32 0
// CHECK-NEXT: store i8** [[WT]], i8*** [[T0]], align 8
// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds [[SET]]* [[LOCAL]], i32 0, i32 1
// CHECK-NEXT: [[T1:%.*]] = getelementptr inbounds [[SET]]* [[RESULT]], i32 0, i32 1
// CHECK-NEXT: [[T2:%.*]] = getelementptr inbounds i8** [[WT]], i32 1
// CHECK-NEXT: [[T3:%.*]] = load i8** [[T2]], align 8
// CHECK-NEXT: [[FN:%.*]] = bitcast i8* [[T3]] to i8* ([[BUFFER]]*, [[BUFFER]]*, i8**)*
// CHECK-NEXT: call i8* [[FN]]([[BUFFER]]* noalias [[T1]], [[BUFFER]]* [[T0]], i8** [[WT]])

// Destroy local.
// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds [[SET]]* [[LOCAL]], i32 0, i32 0
// CHECK-NEXT: [[WT:%.*]] = load i8*** [[T0]], align 8
// CHECK-NEXT: [[T0:%.*]] = getelementptr inbounds [[SET]]* [[LOCAL]], i32 0, i32 1
// CHECK-NEXT: [[T1:%.*]] = load i8** [[WT]], align 8
// CHECK-NEXT: [[FN:%.*]] = bitcast i8* [[T1]] to void ([[BUFFER]]*, i8**)*
// CHECK-NEXT: call void [[FN]]([[BUFFER]]* [[T0]], i8** [[WT]]) nounwind

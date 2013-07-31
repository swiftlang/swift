// RUN: %swift -triple x86_64-apple-darwin10 %s -emit-llvm | FileCheck %s
// REQUIRES: john_to_rewrite_call_irgen

// CHECK: [[TY0:%.*]] = type { i1, [15 x i8] }
// CHECK: [[INT:%.*]] = type { i64 }
// CHECK: [[TY1:%.*]] = type { i8 }

union Ty0 {
  case x(Int)
  case y
}

// CHECK: define { i8*, %swift.refcounted* } @_TO5union3Ty01xFMS0_FSiS0_() {
// FIXME
// CHECK-NOT:  store
// CHECK:      unreachable

// CHECK: define void @_TO5union3Ty01yFMS0_S0_([[TY0]]* sret noalias) {
// FIXME
// CHECK-NOT:  store
// CHECK:      ret void

func f0(t0 : Ty0) -> Int {
  return 0
}
// CHECK: define i64 @_T5union2f0FT2t0OS_3Ty0_Si([[TY0]]* %t0) {

func f1() -> Ty0 {
  return .x(0)
}
// CHECK:    define void @_T5union2f1FT_OS_3Ty0([[TY0]]* sret noalias) {
// CHECK:      call {{.*}} @_TO5union3Ty01xFMS0_FSiS0_
// CHECK:      call void {{.*}}([[TY0]]* sret noalias {{%.*}}, i64 0,
// CHECK-NEXT: ret void

union Ty1 {
  case x
  case y
  case z
}
// CHECK:    define i8 @_TO5union3Ty11xFMS0_S0_() {
// CHECK:      ret i8 0
// CHECK:    define i8 @_TO5union3Ty11yFMS0_S0_() {
// CHECK:      ret i8 1
// CHECK:    define i8 @_TO5union3Ty11zFMS0_S0_() {
// CHECK:      ret i8 2

func f2() -> Ty1 {
  return .x
}
// CHECK:    define i8 @_T5union2f2FT_OS_3Ty1() {
// CHECK:      [[RET:%.*]] = alloca [[TY1]], align 1
// CHECK-NEXT: [[T0:%.*]] = call i8 @_TO5union3Ty11xFMS0_S0_()
// CHECK-NEXT: [[T1:%.*]] = getelementptr inbounds [[TY1]]* [[RET]], i32 0, i32 0
// CHECK-NEXT: store i8 [[T0]], i8* [[T1]], align 1
// CHECK-NEXT: [[T2:%.*]] = getelementptr inbounds [[TY1]]* [[RET]], i32 0, i32 0
// CHECK-NEXT: [[T3:%.*]] = load i8* [[T2]], align 1
// CHECK-NEXT: ret i8 [[T3]]

union Ty2 {
  case val(Ty1)
}
// CHECK:    define {{.*}} @_TO5union3Ty23valFMS0_FOS_3Ty1S0_()
// FIXME
// CHECK:      unreachable

union Ty3 {
  case val(Int, Int)
}
// CHECK:    define {{.*}} @_TO5union3Ty33valFMS0_FTSiSi_S0_() {
// FIXME
// CHECK:      unreachable

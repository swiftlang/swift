// RUN: %target-swift-frontend %s -emit-ir -O | %FileCheck %s

// REQUIRES: swift_in_compiler

// When no witness methods are called on pack elements, all pack code can be fully eliminated.
// CHECK: define {{.*}} { i64, i64, ptr, double } @"$s19pack_specialization8copyPack2xsxxQp_txxQp_tRvzlFSi_SSSdQP_Tg5Tf8xx_n"(i64 %0, i64 %1, ptr %2, double %3)
// CHECK-NEXT: entry:
// CHECK-NEXT: insertvalue
// CHECK-NEXT: insertvalue
// CHECK-NEXT: insertvalue
// CHECK-NEXT: insertvalue
// CHECK-NEXT: @swift_bridgeObjectRetain
// CHECK-NEXT: ret { i64, i64, ptr, double }
@inline(never)
func copyPack<each A>(xs: repeat each A) -> (repeat each A) {
  return (repeat each xs);
}

public func copyPackCaller() -> (Int, String, Double) {
  return copyPack(xs: 1, "two", 3.0)
}

// The pack specialization pass alone is not enough to eliminate packs when witness methods are called.
// CHECK: define {{.*}} i64 @"$s19pack_specialization11addTogether2xsxxQp_txxQp_tRvzSjRzlFSi_QP_Tg5Tf8xx_n"(i64 %0)
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[IN_STACK:%[0-9]+]] = alloca %TSi, align 8
// CHECK-NEXT:    [[OUT_STACK:%[0-9]+]] = alloca %TSi, align 8
// CHECK:         store i64 %0, ptr [[IN_STACK]], align 8
// CHECK:         call swiftcc void @"$ss18AdditiveArithmeticP1poiyxx_xtFZTj"
// CHECK:         [[RESULT:%[0-9]+]] = load i64, ptr [[OUT_STACK]]
// CHECK:         ret i64 [[RESULT]]
@inline(never)
func addTogether<each A: Numeric>(xs: repeat each A) -> (repeat each A) {
  return (repeat (each xs + each xs))
}

public func addTogetherCaller() -> Int {
  return addTogether(xs: 1)
}

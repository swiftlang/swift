// RUN: %target-swift-frontend %s -emit-ir -O -solver-disable-crash-on-valid-salvage | %FileCheck %s
// RUN: not --crash %target-swift-frontend %s -emit-ir -O -solver-enable-crash-on-valid-salvage

// REQUIRES: swift_in_compiler



// When no witness methods are called on pack elements, all pack code can be fully eliminated.
// CHECK: define {{.*}} { i32, ptr, double } @"$s19pack_specialization8copyPack2xsxxQp_txxQp_tRvzlFs5Int32V_SPys5Int16VGSdQP_Tg5Tf8xx_n"(i32 %0, ptr %1, double %2)
// CHECK-NEXT: entry:
// CHECK-NEXT: insertvalue
// CHECK-NEXT: insertvalue
// CHECK-NEXT: insertvalue
// CHECK-NEXT: ret { i32, ptr, double }
@inline(never)
func copyPack<each A>(xs: repeat each A) -> (repeat each A) {
  return (repeat each xs);
}

public func copyPackCaller(two: UnsafePointer<Int16>) -> (Int32, UnsafePointer<Int16>, Double) {
  return copyPack(xs: 1, two, 3.0)
}

// The pack specialization pass alone is not enough to eliminate packs when witness methods are called.
// CHECK: define {{.*}} i32 @"$s19pack_specialization11addTogether2xsxxQp_txxQp_tRvzSjRzlFs5Int32V_QP_Tg5Tf8xx_n"(i32 %0)
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[IN_STACK:%[0-9]+]] = alloca %Ts5Int32V, align 4
// CHECK-NEXT:    [[OUT_STACK:%[0-9]+]] = alloca %Ts5Int32V, align 4
// CHECK:         store i32 %0, ptr [[IN_STACK]], align 4
// CHECK:         call swiftcc void @"$ss18AdditiveArithmeticP1poiyxx_xtFZTj"
// CHECK:         [[RESULT:%[0-9]+]] = load i32, ptr [[OUT_STACK]]
// CHECK:         ret i32 [[RESULT]]
@inline(never)
func addTogether<each A: Numeric>(xs: repeat each A) -> (repeat each A) {
  return (repeat (each xs + each xs))
}

public func addTogetherCaller() -> Int32 {
  return addTogether(xs: 1)
}

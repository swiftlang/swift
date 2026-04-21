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

// When iterating over a single pack, all pack code can be fully eliminated.
// CHECK: define {{.*}} i32 @"$s19pack_specialization11addTogether2xsxxQp_txxQp_tRvzSjRzlFs5Int32V_QP_Tg5Tf8xx_n"(i32 %0)
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[SUM:%[0-9]+]] = tail call { i32, i1 } @llvm.sadd.with.overflow.i32(i32 %0, i32 %0)
// CHECK:         [[RESULT:%[0-9]+]] = extractvalue { i32, i1 } %1, 0
// CHECK:         ret i32 [[RESULT]]
@inline(never)
func addTogether<each A: Numeric>(xs: repeat each A) -> (repeat each A) {
  return (repeat (each xs + each xs))
}

public func addTogetherCaller() -> Int32 {
  return addTogether(xs: 1)
}

// When iterating over multiple packs simultaneously, some pack code cannot be
// eliminated, so we still access the witness table in the resulting IR.
// CHECK: define {{.*}} { i64, i64 } @"$s19pack_specialization8zipPacks2xs2zsx_q_txQp_txxQp_q_xQptRvzRv_SzRzq_RhzSzR_r0_lFs5Int32V_s6UInt32VQP_s4Int8V_s5Int16VQPTg5Tf8xxx_n"(i32 %0, i32 %1, i8 %2, i16 %3)
// CHECK: @"$ss5Int32VABSzsWl"()
// CHECK: @swift_getTupleTypeMetadata2{{.*}} @"$ss5Int32VN"
// CHECK: @swift_getAssociatedTypeWitness{{.*}}
// CHECK: call{{.*}}@"$sSj1moiyxx_xtFZTj"
@inline(never)
func zipPacks<each A: BinaryInteger, each B: BinaryInteger>(xs: repeat each A, zs: repeat each B) -> (repeat (each A, each B)) {
  return (repeat (each xs * 2, each zs * 2))
}

public func zipAddPacksCaller(x: Int32, y: UInt32, a: Int8, b: Int16)
  -> ((Int32, Int8), (UInt32, Int16)) {
  return zipPacks(xs: x, y, zs: a, b)
}

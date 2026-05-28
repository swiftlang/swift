// RUN: %target-swift-frontend %s -emit-ir -O -solver-disable-crash-on-valid-salvage | %FileCheck %s
// RUN: not --crash %target-swift-frontend %s -emit-ir -O -solver-enable-crash-on-valid-salvage

// REQUIRES: swift_in_compiler

// Some tests started failing for 32-bit since #88914.
// REQUIRES: PTRSIZE=64

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

// With open_pack_element elimination, witness methods on pack elements resolve
// to concrete types, enabling full devirtualization and optimization.
// CHECK:       define {{.*}} i32 @"$s19pack_specialization11addTogether2xsxxQp_txxQp_tRvzSjRzlFs5Int32V_QP_Tg5Tf8xx_n"(i32 %0)
// CHECK-NEXT:  entry:
// CHECK-NOT:     call {{.*}} @"$ss18AdditiveArithmeticP1poiyxx_xtFZTj"
// CHECK:         call { i32, i1 } @llvm.sadd.with.overflow.i32(i32 %0, i32 %0)
// CHECK:         ret i32
@inline(never)
func addTogether<each A: Numeric>(xs: repeat each A) -> (repeat each A) {
  return (repeat (each xs + each xs))
}

public func addTogetherCaller() -> Int32 {
  return addTogether(xs: 1)
}

// Dependent/member types (e.g. each T.IntegerLiteralType) must also resolve to
// concrete types when the open_pack_element is eliminated.
// CHECK-LABEL: define {{.*}} double @"$s19pack_specialization8addOneTo4argsxxQp_txxQp_tRvzSjRzlFSd_QP_Tg5Tf8xx_n"(double %0) {{.*}} {
// CHECK-NEXT:  entry:
// CHECK-NOT:     call{{.*}}@"$ss18AdditiveArithmeticP1poiyxx_xtFZTj"
// CHECK:         [[RESULT:%[0-9]+]] = fadd double %0, 1.000000e+00
// CHECK:         ret double
// CHECK: }

// CHECK-LABEL: define {{.*}} { i64, double, i8 } @"$s19pack_specialization8addOneTo4argsxxQp_txxQp_tRvzSjRzlFSi_Sds4Int8VQP_Tg5Tf8xx_n"(i64 %0, double %1, i8 %2) {{.*}} {
// CHECK:         @llvm.sadd.with.overflow.i64(i64 %0, i64 1)
// CHECK:         @llvm.sadd.with.overflow.i8(i8 %2, i8 1)
// CHECK:         fadd double %1, 1.000000e+00
// CHECK: }
@inline(never)
func addOneTo<each T: Numeric>(args arg: repeat each T) -> (repeat each T) {
  return (repeat 1 + each arg)
}

public func addOneToCaller(y: Double) -> Double {
  return addOneTo(args: y)
}

public func addOneToMultiCaller(x: Int, y: Double, z: Int8) -> (Int, Double, Int8) {
  return addOneTo(args: x, y, z)
}

// Iterating over multiple packs simultaneously.
// CHECK: define {{.*}} { i64, i64 } @"$s19pack_specialization8zipPacks2xs2zsx_q_txQp_txxQp_q_xQptRvzRv_SzRzq_RhzSzR_r0_lFs5Int32V_s6UInt32VQP_s4Int8V_s5Int16VQPTg5Tf8xxx_n"(i32 %0, i32 %1, i8 %2, i16 %3)
// CHECK-NOT: alloca
// CHECK:   shl nsw i16 %3, 1
// CHECK:   shl nuw i32 %1, 1
// CHECK:   shl nsw i8 %2, 1
// CHECK:   shl nsw i32 %0, 1
// CHECK: }
@inline(never)
func zipPacks<each A: BinaryInteger, each B: BinaryInteger>(xs: repeat each A, zs: repeat each B) -> (repeat (each A, each B)) {
  return (repeat (each xs * 2, each zs * 2))
}

public func zipAddPacksCaller(x: Int32, y: UInt32, a: Int8, b: Int16)
  -> ((Int32, Int8), (UInt32, Int16)) {
  return zipPacks(xs: x, y, zs: a, b)
}

// When applying a generic function to each element of a pack, the callee can be
// specialized if the (variadic generic) caller is specialized.
func numericOp<T: BinaryInteger>(_ x: T) -> T {
  if x <= 1 {
    return x
  } else {
    let (p, q) = numericLoop(x - 1, x - 2)
    return p + q
  }
}

// CHECK: define {{.*}} i64 @"$s19pack_specialization11numericLoopyxxQp_txxQpRvzSzRzlFs5Int32V_ADQP_Tg5Tf8xx_n"(i32 %0, i32 %1) {{.*}} {
// CHECK: [[SP1:%[0-9]+]] = add nsw i32 %0, -1
// CHECK: [[SP2:%[0-9]+]] = add nsw i32 %0, -2
// CHECK: [[SP_RESULT:%[0-9]+]] = tail call {{.*}} @"$s19pack_specialization11numericLoopyxxQp_txxQpRvzSzRzlFs5Int32V_ADQP_Tg5Tf8xx_n"(i32 [[SP1]], i32 [[SP2]])
// CHECK-NEXT: [[SP_RESULT1:%[^ ]+]] = trunc i64 [[SP_RESULT]] to i32
// CHECK-NEXT: [[SP_RESULT2_64:%[^ ]+]] = lshr i64 [[SP_RESULT]], 32
// CHECK-NEXT: [[SP_RESULT2:%[^ ]+]] = trunc nuw i64 [[SP_RESULT2_64]] to i32
// CHECK: tail call { i32, i1 } @llvm.sadd.with.overflow.i32(i32 [[SP_RESULT1]], i32 [[SP_RESULT2]])
// CHECK: }
@inline(never)
func numericLoop<each T: BinaryInteger>(_ xs: repeat each T) -> (repeat each T) {
  return (repeat numericOp(each xs))
}

// CHECK: define {{.*}} i64 @"$s19pack_specialization15callNumericLoops5Int32V_ADtyF"() {{.*}} {
// CHECK: tail call
// CHECK: }
public func callNumericLoop() -> (Int32, Int32) {
  numericLoop(39, 39)
}

// Issue #82000: Combining pointers and parameter packs.

// CHECK: define {{.*}} i32 @"$s19pack_specialization12applyPointer5input2ops5Int32VSPyxGxQp_AFxxQpXEtRvzlF"(ptr noalias readonly captures(none) %0, ptr readonly captures(none) %1, ptr %2, i64 %3, ptr %"each T") {{.*}} {
// CHECK: }
public func applyPointer<each T>(input: repeat UnsafePointer<each T>, op: (repeat each T) -> Int32) -> Int32 {
  op(repeat (each input).pointee)
}

// CHECK: define {{.*}} i32 @"$s19pack_specialization4tests5Int32VyF"() {{.*}} {
// CHECK-NEXT: "$s19pack_specialization4tests5Int32VyFADSPyADGXEfU_.exit":
// CHECK-NEXT:   ret i32 54
// CHECK-NEXT: }
public func test() -> Int32 {
    withUnsafePointer(to: 27) {
        applyPointer(input: $0, $0) { ($0 + $1) }
    }
}

// CHECK: define {{.*}} i32 @"$s19pack_specialization11applyDirect5input2ops5Int32VxxQp_AFxxQpXEtRvzlF"(ptr noalias readonly captures(none) %0, ptr readonly captures(none) %1, ptr %2, i64 %3, ptr %"each T") {{.*}} {
// CHECK: }
public func applyDirect<each T>(input: repeat each T, op: (repeat each T) -> Int32) -> Int32 {
  op(repeat (each input))
}

// CHECK: define {{.*}} i32 @"$s19pack_specialization10testDirects5Int32VyF"() {{.*}} {
// CHECK-NEXT: entry:
// CHECK-NEXT:   ret i32 54
// CHECK-NEXT: }
public func testDirect() -> Int32 {
    applyDirect(input: 27, 27) { ($0 + $1) }
}

// Eliminating @pack_element types when a pack loop cannot be unrolled.
// This only works if every element of the pack is the same type.
//
// This test produces a loop with 33 iterations, exceeding the upper bound in
// the LoopUnroll pass.


func addTogetherInlinable<each A: Numeric>(xs: repeat each A) -> (repeat each A) {
  return (repeat (each xs + each xs))
}

// CHECK:         define {{.*}} void @"$s19pack_specialization13addTogether33s5Int32V_A32DtyF"(ptr {{.*}} sret(<{ %Ts5Int32V, %Ts5Int32V, %Ts5Int32V, %Ts5Int32V, %Ts5Int32V, %Ts5Int32V, %Ts5Int32V, %Ts5Int32V, %Ts5Int32V, %Ts5Int32V, %Ts5Int32V, %Ts5Int32V, %Ts5Int32V, %Ts5Int32V, %Ts5Int32V, %Ts5Int32V, %Ts5Int32V, %Ts5Int32V, %Ts5Int32V, %Ts5Int32V, %Ts5Int32V, %Ts5Int32V, %Ts5Int32V, %Ts5Int32V, %Ts5Int32V, %Ts5Int32V, %Ts5Int32V, %Ts5Int32V, %Ts5Int32V, %Ts5Int32V, %Ts5Int32V, %Ts5Int32V, %Ts5Int32V }>) {{.*}} %0) {{.*}} {
// CHECK:           [[INPUT_PACK:%[0-9]+]] =  alloca [33 x ptr]
// CHECK-COUNT-33:  alloca %Ts5Int32V
// CHECK:           store ptr {{.*}}, ptr [[INPUT_PACK]]
// CHECK:           [[DYN_IDX:%[0-9]+]] = phi
// CHECK-NEXT:      [[DYN_ADDR:%[0-9]+]] = getelementptr {{.*}} [[INPUT_PACK]]
// CHECK-NEXT:      [[ELEM_ADDR:%[0-9]+]] = load ptr, ptr [[DYN_ADDR]]
// CHECK-NEXT:      [[ELEM:%[0-9]+]] = load i32, ptr [[ELEM_ADDR]]
// CHECK-NEXT:      [[DOUBLED:%[0-9]+]] = call { i32, i1 } @llvm.sadd.with.overflow.i32(i32 [[ELEM]], i32 [[ELEM]])
// CHECK-NOT:       @llvm.sadd.with.overflow.i32
// CHECK:         }

public func addTogether33() -> (Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32) {
  return addTogetherInlinable(xs: 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33)
}

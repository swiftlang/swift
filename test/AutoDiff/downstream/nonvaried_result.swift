// RUN: %target-swift-frontend -Xllvm -sil-print-after=differentiation %s -emit-sil -o /dev/null 2>&1 | %FileCheck %s
// RUN: %target-run-simple-swift
// TODO: Test forward-mode differentiation when it supports control flow.
// UN: %target-run-simple-swift-forward-mode-differentiation
// REQUIRES: executable_test

// Test differentiation edge case: functions with non-varied results.
// The differentials of these functions should return zero.
// The pullbacks of these functions should return zero with respect to the
// parameters for which the result is non-varying.

import StdlibUnittest
import DifferentiationUnittest

var NonVariedResultTests = TestSuite("TestCaseTests")

NonVariedResultTests.testWithLeakChecking("SingleBasicBlock") {
  @differentiable(wrt: y)
  func simple(_ x: Tracked<Float>, _ y: Tracked<Float>) -> Tracked<Float> {
    return x
  }
  expectEqual(0, gradient(at: 3) { simple(10, $0) })
  expectEqual((1, 0), gradient(at: 3, 4, in: simple))
}

// CHECK-LABEL: sil private [ossa] @AD__${{.*}}simple{{.*}}pullback_src_0_wrt_1 : $@convention(thin) (@guaranteed Tracked<Float>, @owned _AD__$s4nullyycfU_6simpleL_y23DifferentiationUnittest7TrackedVySfGAF_AFtF_bb0__PB__src_0_wrt_1) -> @owned Tracked<Float> {
// CHECK: bb0([[SEED:%.*]] : @guaranteed $Tracked<Float>, [[PB_STRUCT:%.*]] : [[PB_STRUCT_TYPE:.*]]):
// CHECK:   [[BUF:%.*]] = alloc_stack $Tracked<Float>
// CHECK:   [[ZERO_FN:%.*]] = witness_method $Tracked<Float>, #AdditiveArithmetic.zero!getter : <Self where Self : AdditiveArithmetic> (Self.Type) -> () -> Self : $@convention(witness_method: AdditiveArithmetic) <τ_0_0 where τ_0_0 : AdditiveArithmetic> (@thick τ_0_0.Type) -> @out τ_0_0
// CHECK:   [[METATYPE:%.*]] = metatype $@thick Tracked<Float>.Type
// CHECK:   {{%.*}} = apply [[ZERO_FN]]<Tracked<Float>>([[BUF]], [[METATYPE]]) : $@convention(witness_method: AdditiveArithmetic) <τ_0_0 where τ_0_0 : AdditiveArithmetic> (@thick τ_0_0.Type) -> @out τ_0_0
// CHECK:   [[ZERO_VALUE:%.*]] = load [take] [[BUF]] : $*Tracked<Float>
// CHECK:   dealloc_stack [[BUF]] : $*Tracked<Float>
// CHECK:   return [[ZERO_VALUE]]

NonVariedResultTests.testWithLeakChecking("SingleBasicBlockGeneric") {
  // Test zero wrt multiple arguments.
  @differentiable(wrt: (x, y, z))
  func simpleGeneric<T: Differentiable>(
    _ x: T, _ y: T, _ z: Tracked<Float>
  ) -> T where T == T.TangentVector {
    return .zero
  }
  expectEqual((0, 0, 0), gradient(at: 3, 4, 5) { simpleGeneric($0, $1, $2) })
}

// CHECK-LABEL: sil private [ossa] @AD__${{.*}}simpleGeneric{{.*}}pullback_src_0_wrt_0_1_2{{.*}} : $@convention(thin) <τ_0_0 where τ_0_0 : Differentiable, τ_0_0 == τ_0_0.TangentVector> (@in_guaranteed τ_0_0, @owned _AD__$s4nullyycfU0_13simpleGenericL_yxx_x23DifferentiationUnittest7TrackedVySfGts14DifferentiableRz13TangentVectorsAGPQzRszlF_bb0__PB__src_0_wrt_0_1_2<τ_0_0>) -> (@out τ_0_0, @out τ_0_0, @owned Tracked<Float>) {
// CHECK: bb0([[DX:%.*]] : $*τ_0_0, [[DY:%.*]] : $*τ_0_0, [[SEED:%.*]] : $*τ_0_0, [[PB_STRUCT:%.*]] : [[PB_STRUCT_TYPE:.*]]):
// CHECK:   [[ZERO_FN_X:%.*]] = witness_method $τ_0_0, #AdditiveArithmetic.zero!getter : <Self where Self : AdditiveArithmetic> (Self.Type) -> () -> Self : $@convention(witness_method: AdditiveArithmetic) <τ_0_0 where τ_0_0 : AdditiveArithmetic> (@thick τ_0_0.Type) -> @out τ_0_0
// CHECK:   [[METATYPE_X:%.*]] = metatype $@thick τ_0_0.Type
// CHECK:   {{.*}} = apply [[ZERO_FN_X]]<τ_0_0>([[DX]], [[METATYPE_X]]) : $@convention(witness_method: AdditiveArithmetic) <τ_0_0 where τ_0_0 : AdditiveArithmetic> (@thick τ_0_0.Type) -> @out τ_0_0
// CHECK:   [[ZERO_FN_Y:%.*]] = witness_method $τ_0_0, #AdditiveArithmetic.zero!getter : <Self where Self : AdditiveArithmetic> (Self.Type) -> () -> Self : $@convention(witness_method: AdditiveArithmetic) <τ_0_0 where τ_0_0 : AdditiveArithmetic> (@thick τ_0_0.Type) -> @out τ_0_0
// CHECK:   [[METATYPE_Y:%.*]] = metatype $@thick τ_0_0.Type
// CHECK:   {{.*}} = apply [[ZERO_FN_Y:%.*]]<τ_0_0>([[DY]], [[METATYPE_Y]]) : $@convention(witness_method: AdditiveArithmetic) <τ_0_0 where τ_0_0 : AdditiveArithmetic> (@thick τ_0_0.Type) -> @out τ_0_0
// CHECK:   [[BUF_Z:%.*]] = alloc_stack $Tracked<Float>
// CHECK:   [[ZERO_FN_Z:%.*]] = witness_method $Tracked<Float>, #AdditiveArithmetic.zero!getter : <Self where Self : AdditiveArithmetic> (Self.Type) -> () -> Self : $@convention(witness_method: AdditiveArithmetic) <τ_0_0 where τ_0_0 : AdditiveArithmetic> (@thick τ_0_0.Type) -> @out τ_0_0
// CHECK:   [[METATYPE_Z:%.*]] = metatype $@thick Tracked<Float>.Type
// CHECK:   {{%.*}} = apply [[ZERO_FN_Z]]<Tracked<Float>>([[BUF_Z]], [[METATYPE_Z]]) : $@convention(witness_method: AdditiveArithmetic) <τ_0_0 where τ_0_0 : AdditiveArithmetic> (@thick τ_0_0.Type) -> @out τ_0_0
// CHECK:   [[ZERO_VALUE_Z:%.*]] = load [take] [[BUF_Z]] : $*Tracked<Float>
// CHECK:   dealloc_stack [[BUF_Z]] : $*Tracked<Float>
// CHECK:   return [[ZERO_VALUE_Z]]

NonVariedResultTests.testWithLeakChecking("Conditionals") {
  @differentiable(wrt: y)
  func `if`(_ x: Tracked<Float>, _ y: Tracked<Float>) -> Tracked<Float> {
    if x > 0 {}
    return x
  }
  expectEqual(0, gradient(at: 3) { `if`(10, $0) })
  expectEqual((1, 0), gradient(at: 3, 4, in: `if`))

  @differentiable(wrt: y)
  func `guard`(_ x: Tracked<Float>, _ y: Tracked<Float>) -> Tracked<Float> {
    guard x > 0 else { return x }
    return x
  }
  expectEqual(0, gradient(at: 3) { x in `guard`(10, x) })
  expectEqual((1, 0), gradient(at: 3, 4, in: `guard`))

  @differentiable(wrt: y)
  func `switch`(_ x: Tracked<Float>, _ y: Tracked<Float>) -> Tracked<Float> {
    switch x.value {
    case 0: break
    default: break
    }
    return x
  }
  expectEqual(0, gradient(at: 3) { `switch`(10, $0) })
  expectEqual((1, 0), gradient(at: 3, 4, in: `switch`))
}

NonVariedResultTests.testWithLeakChecking("Loops") {
  @differentiable(wrt: y)
  func `for`(_ x: Tracked<Float>, _ y: Tracked<Float>) -> Tracked<Float> {
    for i in 0..<10 {}
    return x
  }
  expectEqual(0, gradient(at: 3) { `for`(10, $0) })
  expectEqual((1, 0), gradient(at: 3, 4, in: `for`))

  @differentiable(wrt: y)
  func `while`(_ x: Tracked<Float>, _ y: Tracked<Float>) -> Tracked<Float> {
    while 0 < 0 {}
    return x
  }
  expectEqual(0, gradient(at: 3) { `while`(10, $0) })
  expectEqual((1, 0), gradient(at: 3, 4, in: `while`))
}

NonVariedResultTests.testWithLeakChecking("Complex") {
  @differentiable(wrt: y)
  func complex(_ x: Tracked<Float>, _ y: Tracked<Float>) -> Tracked<Float> {
    for i in 0..<10 {
      for j in 0..<10 {
        if x > 0 {}
        while 0 < 0 {}
        switch x.value {
        case 0: break
        default: break
        }
      }
    }
    return x + x + x
  }
  expectEqual(0, gradient(at: 3) { complex(10, $0) })
  expectEqual((3, 0), gradient(at: 3, 4, in: complex))
}

// CHECK-LABEL: sil private [ossa] @AD__${{.*}}complex{{.*}}pullback_src_0_wrt_1 : $@convention(thin) (@guaranteed Tracked<Float>, @owned _AD__$s4nullyycfU3_7complexL_y23DifferentiationUnittest7TrackedVySfGAF_AFtF_bb15__PB__src_0_wrt_1) -> @owned Tracked<Float> {
// CHECK: bb0([[SEED:%.*]] : @guaranteed $Tracked<Float>, [[PB_STRUCT:%.*]] : @owned [[PB_STRUCT_TYPE:.*]]):
// CHECK:   destroy_value [[PB_STRUCT]] : [[PB_STRUCT_TYPE]]
// CHECK:   [[BUF:%.*]] = alloc_stack $Tracked<Float>
// CHECK:   [[ZERO_FN:%.*]] = witness_method $Tracked<Float>, #AdditiveArithmetic.zero!getter : <Self where Self : AdditiveArithmetic> (Self.Type) -> () -> Self : $@convention(witness_method: AdditiveArithmetic) <τ_0_0 where τ_0_0 : AdditiveArithmetic> (@thick τ_0_0.Type) -> @out τ_0_0
// CHECK:   [[METATYPE:%.*]] = metatype $@thick Tracked<Float>.Type
// CHECK:   {{%.*}} = apply [[ZERO_FN]]<Tracked<Float>>([[BUF]], [[METATYPE]]) : $@convention(witness_method: AdditiveArithmetic) <τ_0_0 where τ_0_0 : AdditiveArithmetic> (@thick τ_0_0.Type) -> @out τ_0_0
// CHECK:   [[ZERO_VALUE:%.*]] = load [take] [[BUF]] : $*Tracked<Float>
// CHECK:   dealloc_stack [[BUF]] : $*Tracked<Float>
// CHECK:   return [[ZERO_VALUE]]

NonVariedResultTests.testWithLeakChecking("ComplexGeneric") {
  @differentiable(wrt: y)
  func complexGeneric<T: Differentiable>(_ x: T, _ y: T) -> T {
    for i in 0..<10 {
      for j in 0..<10 {
        while 0 < 0 {}
      }
    }
    return x
  }
  expectEqual(0, pullback(at: Tracked<Float>(3)) { complexGeneric(10, $0) }(1))
}

// CHECK-LABEL: sil private [ossa] @AD__${{.*}}complexGeneric{{.*}}pullback_src_0_wrt_1{{.*}} : $@convention(thin) <τ_0_0 where τ_0_0 : Differentiable> (@in_guaranteed τ_0_0.TangentVector, @owned _AD__$s4nullyycfU4_14complexGenericL_yxx_xts14DifferentiableRzlF_bb9__PB__src_0_wrt_1<τ_0_0>) -> @out τ_0_0.TangentVector {
// CHECK: bb0([[DY:%.*]] : $*τ_0_0.TangentVector, [[SEED:%.*]] : $*τ_0_0.TangentVector, [[PB_STRUCT:%.*]] : @owned [[PB_STRUCT_TYPE:.*]]):
// CHECK:   destroy_value [[PB_STRUCT]] : [[PB_STRUCT_TYPE]]
// CHECK:   [[ZERO_FN:%.*]] = witness_method $τ_0_0.TangentVector, #AdditiveArithmetic.zero!getter : <Self where Self : AdditiveArithmetic> (Self.Type) -> () -> Self : $@convention(witness_method: AdditiveArithmetic) <τ_0_0 where τ_0_0 : AdditiveArithmetic> (@thick τ_0_0.Type) -> @out τ_0_0
// CHECK:   [[METATYPE:%.*]] = metatype $@thick τ_0_0.TangentVector.Type
// CHECK:   {{%.*}} = apply [[ZERO_FN]]<τ_0_0.TangentVector>([[DY]], [[METATYPE]]) : $@convention(witness_method: AdditiveArithmetic) <τ_0_0 where τ_0_0 : AdditiveArithmetic> (@thick τ_0_0.Type) -> @out τ_0_0
// CHECK:   [[VOID:%.*]] = tuple ()
// CHECK:   return [[VOID]]

runAllTests()

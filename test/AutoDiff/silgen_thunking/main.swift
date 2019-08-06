// RUN: %target-swift-frontend -emit-silgen -verify %s %S/../Inputs/silgen_thunking_other_module.swift | %FileCheck %s

// RUN: %empty-directory(%t)
// RUN: %target-build-swift %S/../Inputs/silgen_thunking_other_module.swift %s -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out

// REQUIRES: executable_test

import StdlibUnittest
import DifferentiationUnittest

var DerivativeSILGenThunkTests = TestSuite("DerivativeSILGenThunks")

// TF-619: Test cross-module import of `@differentiable` methods with
// self-ordering thunks.
DerivativeSILGenThunkTests.testWithLeakChecking("CrossModuleMethodSelfReorderingThunk") {
  expectEqual(1, gradient(at: 0) { x in TF_619().foo(x) })
}

// TF-698: Test thunks that perform self-ordering but not reabstraction.
struct TF_698 : Differentiable & AdditiveArithmetic {
  var x: Tracked<Float>
  init(_ x: Tracked<Float>) {
    self.x = x
  }

  @differentiable(jvp: jvpMultiplied, vjp: vjpMultiplied)
  func multiplied(with other: Self) -> Self {
    return Self(x: x * other.x)
  }

  // Helper function to shorten JVP/VJP definitions.
  static func *(lhs: Self, rhs: Self) -> Self {
    return lhs.multiplied(with: rhs)
  }

  func jvpMultiplied(with other: Self) -> (Self, (Self, Self) -> Self) {
    return (multiplied(with: other), { dself, dother in
      self * dother + other * dself
    })
  }

  func vjpMultiplied(with other: Self) -> (Self, (Self) -> (Self, Self)) {
    return (multiplied(with: other), { v in
      (v * other, v * self)
    })
  }
}

// CHECK-LABEL: sil hidden @AD__$s4main6TF_698V10multiplied4withA2C_tF__vjp_src_0_wrt_0_1 : $@convention(method) (@guaranteed TF_698, @guaranteed TF_698) -> (@owned TF_698, @owned @callee_guaranteed (@guaranteed TF_698) -> (@owned TF_698, @owned TF_698))
// CHECK: bb0([[X:%.*]] : $TF_698, [[Y:%.*]] : $TF_698):
// CHECK: [[VJP:%.*]] = function_ref @$s4main6TF_698V13vjpMultiplied4withAC_AC_ACtACctAC_tF
// CHECK: [[VJP_RESULT:%.*]] = apply [[VJP]]([[X]], [[Y]])
// CHECK: [[VJP_ORIG_RESULT:%.*]] = tuple_extract [[VJP_RESULT]] : {{.*}}, 0
// CHECK: [[PB:%.*]] = tuple_extract [[VJP_RESULT]] : {{.*}}, 1
// CHECK: [[PB_SELF_REORDER_THUNK:%.*]] = function_ref @AD__$s4main6TF_698VA2CIeggoo_A3CIeggoo_TR_pullback_self_reordering_thunk
// CHECK: [[THUNKED_PB:%.*]] = partial_apply [callee_guaranteed] [[PB_SELF_REORDER_THUNK]]([[PB]])
// CHECK: [[RESULT:%.*]] = tuple ([[VJP_ORIG_RESULT]] : $TF_698, [[THUNKED_PB]] : {{.*}})
// CHECK: return [[RESULT]]

// CHECK-LABEL: sil shared [transparent] [serialized] [reabstraction_thunk] [ossa] @AD__$s4main6TF_698VA2CIeggoo_A3CIeggoo_TR_pullback_self_reordering_thunk : $@convention(thin) (@guaranteed TF_698, @guaranteed @callee_guaranteed (@guaranteed TF_698) -> (@owned TF_698, @owned TF_698))
// CHECK: bb0([[SEED:%.*]] : @guaranteed $TF_698, [[PB:%.*]] : @guaranteed $@callee_guaranteed (@guaranteed TF_698) -> (@owned TF_698, @owned TF_698)):
// CHECK: [[PB_RESULT:%.*]] = apply [[PB]]([[SEED]])
// CHECK: ([[X_ADJ:%.*]], [[Y_ADJ:%.*]]) = destructure_tuple %2 : $(TF_698, TF_698)
// CHECK: [[RESULT:%.*]] = tuple ([[Y_ADJ]] : $TF_698, [[X_ADJ]] : $TF_698)
// CHECK: return [[RESULT]]

DerivativeSILGenThunkTests.testWithLeakChecking("SelfReorderingNonReabstractingThunk") {
  let v = TF_698(1)
  expectEqual((TF_698(10), TF_698(3)),
              pullback(at: TF_698(3), TF_698(10)) { x, y in x.multiplied(with: y) }(v))
}

runAllTests()

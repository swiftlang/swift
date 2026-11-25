// RUN: %target-swift-frontend -O -sil-verify-all -emit-sil -module-name src %s -o - | %FileCheck %s

import _Differentiation

public struct BranchingMethod: Differentiable {
  var stored: Float
  var optional: Float?

  init(stored: Float, optional: Float?) {
    self.stored = stored
    self.optional = optional
  }

  @differentiable(reverse)
  public func method() -> Float {
    let c: BranchingMethod
    do {
      let tmp = BranchingMethod(stored: 1 * stored, optional: optional)
      let tuple = (tmp, tmp)
      c = tuple.0
    }
    let ret: Float
    if let x = c.optional {
      ret = x * c.stored
    } else {
      ret = 1 * c.stored
    }
    return 1 * ret * ret
  }
}

// CHECK-LABEL:     {{^}}// reverse-mode derivative of condTuple(_:)
// CHECK-NOT:         partial_apply
// CHECK:             // function_ref specialized pullback of condTuple(_:)
// CHECK-NEXT:        %[[#PULLBACK:]] = function_ref @$s3src9condTupleyS2fFTJpSpSr067$sSf16_DifferentiationE7_vjpAdd3lhs3rhsSf5value_Sf_SftSfc8pullbacktk1_l5FZSf_L6SfcfU_0de1_f4E12_h16Subtract3lhs3rhsj1_k1_lmk1_ln1_lO2U_ACTf1nnccc_n00154_28$s3src9condTupleyS2fFTJrSpSr$s3src038_AD__$s3src9condTupleyS2fF_bb3__Pred__A60_0_wrt_0_spec_bb2_1_bb1_1_2_116b82c469264a003ec8a8e7c401b544OD_zBAGFdjCedeTf1nb_n : $@convention(thin) (Float, @owned _AD__$s3src9condTupleyS2fF_bb3__Pred__src_0_wrt_0_spec_bb2_1_bb1_1_2_116b82c469264a003ec8a8e7c401b544) -> Float
// CHECK-NEXT:        %[[#BOUND:]] = partial_apply [callee_guaranteed] %[[#PULLBACK]](%[[#]])
// CHECK-NEXT:        %[[#RESULT:]] = tuple (%[[#]], %[[#BOUND]])
// CHECK-NEXT:        return %[[#RESULT]]

@differentiable(reverse)
public func condTuple(_ x: Float) -> Float {
  var y: (Float, Float) = (x, x)
  var z: (Float, Float) = (x + x, x - x)
  if x > 0 {
    let w = (x, x)
    y.0 = w.1
    y.1 = w.0
    z.0 = z.0 - y.0
    z.1 = z.1 + y.0
  } else {
    z = (1 * x, x)
  }
  return y.0 + y.1 - z.0 + z.1
}

@differentiable(reverse)
func throwing(input: Float) throws -> Float {
  return input
}

// CHECK-LABEL:     {{^}}// reverse-mode derivative of catching(input:)
// CHECK:             // function_ref pullback of catching(input:)
// CHECK-NEXT:        %[[#CATCH_PB:]] = function_ref @$s3src8catching5inputS2f_tFTJpSpSr :
// CHECK-NEXT:        %[[#]] = partial_apply {{.*}} %[[#CATCH_PB]](%[[#]]) : $@convention(thin) (Float, @owned _AD__$s3src8catching5inputS2f_tF_bb4__Pred__src_0_wrt_0) -> Float
// CHECK:           } // end sil function

@differentiable(reverse)
public func catching(input: Float) -> Float {
  do {
    if input > 1 {
      return try throwing(input: input)
    }
    return 2
  } catch {
    return 0
  }
}

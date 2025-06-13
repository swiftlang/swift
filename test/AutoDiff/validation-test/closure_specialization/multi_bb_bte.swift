/// Multi basic block VJP, pullback accepting branch tracing enum argument.

// REQUIRES: executable_test

// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -o %t/none.out -Onone
// RUN: %target-build-swift %s -o %t/opt.out  -O
// RUN: %target-run %t/none.out
// RUN: %target-run %t/opt.out

// RUN: %target-swift-frontend -emit-sil %s -O -o %t/out.sil
// RUN: cat %t/out.sil | %FileCheck %s --check-prefix=CHECK1
// RUN: cat %t/out.sil | %FileCheck %s --check-prefix=CHECK2
// RUN: cat %t/out.sil | %FileCheck %s --check-prefix=CHECK3

import DifferentiationUnittest
import StdlibUnittest

var AutoDiffClosureSpecMultiBBBTETests = TestSuite("AutoDiffClosureSpecMultiBBBTE")

AutoDiffClosureSpecMultiBBBTETests.testWithLeakChecking("Test1") {
  // CHECK1-LABEL: {{^}}// reverse-mode derivative of mul42 #1 (_:)
  // CHECK1-NEXT:  sil private @$s3outyycfU_5mul42L_yS2fSgFTJrSpSr : $@convention(thin) (Optional<Float>) -> (Float, @owned @callee_guaranteed (Float) -> Optional<Float>.TangentVector) {
  // CHECK1:         %[[#A12:]] = function_ref @$s3outyycfU_5mul42L_yS2fSgFTJpSpSr073$sSf16_DifferentiationE12_vjpMultiply3lhs3rhsSf5value_Sf_SftSfc8pullbacktj1_k5FZSf_K6SfcfU_S2fTf1nnc_n : $@convention(thin) (Float, @owned _AD__$s3outyycfU_5mul42L_yS2fSgF_bb2__Pred__src_0_wrt_0, Float, Float) -> Optional<Float>.TangentVector
  // CHECK1:         %[[#A13:]] = partial_apply [callee_guaranteed] %[[#A12]](%[[#]], %[[#]], %[[#]]) : $@convention(thin) (Float, @owned _AD__$s3outyycfU_5mul42L_yS2fSgF_bb2__Pred__src_0_wrt_0, Float, Float) -> Optional<Float>.TangentVector
  // CHECK1:         %[[#A14:]] = tuple (%[[#]], %[[#A13]])
  // CHECK1:         return %[[#A14]]
  // CHECK1:       } // end sil function '$s3outyycfU_5mul42L_yS2fSgFTJrSpSr'

  // CHECK1-NONE:  {{^}}// pullback of mul42
  // CHECK1:       {{^}}// specialized pullback of mul42
  // CHECK1:       sil private @$s3outyycfU_5mul42L_yS2fSgFTJpSpSr073$sSf16_DifferentiationE12_vjpMultiply3lhs3rhsSf5value_Sf_SftSfc8pullbacktj1_k5FZSf_K6SfcfU_S2fTf1nnc_n : $@convention(thin) (Float, @owned _AD__$s3outyycfU_5mul42L_yS2fSgF_bb2__Pred__src_0_wrt_0, Float, Float) -> Optional<Float>.TangentVector {

  @differentiable(reverse)
  func mul42(_ a: Float?) -> Float {
    let b = 42 * a!
    return b
  }

  expectEqual((-84, 42), valueWithGradient(at: -2, of: mul42))
  expectEqual((0, 42), valueWithGradient(at: 0, of: mul42))
  expectEqual((42, 42), valueWithGradient(at: 1, of: mul42))
  expectEqual((210, 42), valueWithGradient(at: 5, of: mul42))
}

AutoDiffClosureSpecMultiBBBTETests.testWithLeakChecking("Test2") {
  // CHECK2-LABEL: {{^}}// reverse-mode derivative of cond_tuple_var #1 (_:)
  // CHECK2-NEXT:  sil private @$s3outyycfU0_14cond_tuple_varL_yS2fFTJrSpSr : $@convention(thin) (Float) -> (Float, @owned @callee_guaranteed (Float) -> Float) {
  // CHECK2:         %[[#E41:]] = function_ref @$s3outyycfU0_14cond_tuple_varL_yS2fFTJpSpSr067$sSf16_DifferentiationE7_vjpAdd3lhs3rhsSf5value_Sf_SftSfc8pullbacktl1_m5FZSf_M6SfcfU_0ef1_g4E12_i16Subtract3lhs3rhsk1_l1_mnl1_mo1_mP2U_ACTf1nnccc_n : $@convention(thin) (Float, @owned _AD__$s3outyycfU0_14cond_tuple_varL_yS2fF_bb3__Pred__src_0_wrt_0) -> Float
  // CHECK2:         %[[#E42:]] = partial_apply [callee_guaranteed] %[[#E41]](%[[#]]) : $@convention(thin) (Float, @owned _AD__$s3outyycfU0_14cond_tuple_varL_yS2fF_bb3__Pred__src_0_wrt_0) -> Float
  // CHECK2:         %[[#E46:]] = tuple (%[[#]], %[[#E42]])
  // CHECK2:         return %[[#E46]]
  // CHECK2:       } // end sil function '$s3outyycfU0_14cond_tuple_varL_yS2fFTJrSpSr'

  // CHECK2-NONE:  {{^}}// pullback of cond_tuple_var
  // CHECK2:       {{^}}// specialized pullback of cond_tuple_var
  // CHECK2:       sil private @$s3outyycfU0_14cond_tuple_varL_yS2fFTJpSpSr067$sSf16_DifferentiationE7_vjpAdd3lhs3rhsSf5value_Sf_SftSfc8pullbacktl1_m5FZSf_M6SfcfU_0ef1_g4E12_i16Subtract3lhs3rhsk1_l1_mnl1_mo1_mP2U_ACTf1nnccc_n : $@convention(thin) (Float, @owned _AD__$s3outyycfU0_14cond_tuple_varL_yS2fF_bb3__Pred__src_0_wrt_0) -> Float {

  func cond_tuple_var(_ x: Float) -> Float {
    // Convoluted function returning `x + x`.
    var y: (Float, Float) = (x, x)
    var z: (Float, Float) = (x + x, x - x)
    if x > 0 {
      var w = (x, x)
      y.0 = w.1
      y.1 = w.0
      z.0 = z.0 - y.0
      z.1 = z.1 + y.0
    } else {
      z = (1 * x, x)
    }
    return y.0 + y.1 - z.0 + z.1
  }

  expectEqual((8, 2), valueWithGradient(at: 4, of: cond_tuple_var))
  expectEqual((-20, 2), valueWithGradient(at: -10, of: cond_tuple_var))
  expectEqual((-2674, 2), valueWithGradient(at: -1337, of: cond_tuple_var))
}

AutoDiffClosureSpecMultiBBBTETests.testWithLeakChecking("Test3") {
  struct Class: Differentiable {
    var stored: Float
    var optional: Float?

    init(stored: Float, optional: Float?) {
      self.stored = stored
      self.optional = optional
    }

    // CHECK3-LABEL: {{^}}// reverse-mode derivative of method()
    // CHECK3-NEXT:  sil private @$s3outyycfU1_5ClassL_V6methodSfyFTJrSpSr : $@convention(method) (Class) -> (Float, @owned @callee_guaranteed (Float) -> Class.TangentVector) {
    // CHECK3:         %[[#C44:]] = function_ref @$s3outyycfU1_5ClassL_V6methodSfyFTJpSpSr073$sSf16_DifferentiationE12_vjpMultiply3lhs3rhsSf5value_Sf_SftSfc8pullbacktk1_l5FZSf_L6SfcfU_S2fAES2fTf1nncc_n : $@convention(thin) (Float, @owned _AD__$s3outyycfU1_5ClassL_V6methodSfyF_bb3__Pred__src_0_wrt_0, Float, Float, Float, Float) -> Class.TangentVector
    // CHECK3:         %[[#C45:]] = partial_apply [callee_guaranteed] %[[#C44]](%[[#]], %[[#]], %[[#]], %[[#]], %[[#]]) : $@convention(thin) (Float, @owned _AD__$s3outyycfU1_5ClassL_V6methodSfyF_bb3__Pred__src_0_wrt_0, Float, Float, Float, Float) -> Class.TangentVector
    // CHECK3:         %[[#C48:]] = tuple (%[[#]], %[[#C45]])
    // CHECK3:         return %[[#C48]]
    // CHECK3:       } // end sil function '$s3outyycfU1_5ClassL_V6methodSfyFTJrSpSr'

    // CHECK3-NONE:  {{^}}// pullback of method
    // CHECK3:       {{^}}// specialized pullback of method()
    // CHECK3:       sil private @$s3outyycfU1_5ClassL_V6methodSfyFTJpSpSr073$sSf16_DifferentiationE12_vjpMultiply3lhs3rhsSf5value_Sf_SftSfc8pullbacktk1_l5FZSf_L6SfcfU_S2fAES2fTf1nncc_n : $@convention(thin) (Float, @owned _AD__$s3outyycfU1_5ClassL_V6methodSfyF_bb3__Pred__src_0_wrt_0, Float, Float, Float, Float) -> Class.TangentVector {

    @differentiable(reverse)
    func method() -> Float {
      let c: Class
      do {
        let tmp = Class(stored: 1 * stored, optional: optional)
        let tuple = (tmp, tmp)
        c = tuple.0
      }
      var ret : Float = 0
      if let x = c.optional {
        ret = x * c.stored
      } else {
        ret = 1 * c.stored
      }
      return 1 * ret * ret
    }
  }

  @differentiable(reverse)
  func methodWrapper(_ x: Class) -> Float {
    x.method()
  }

  expectEqual(
    valueWithGradient(at: Class(stored: 3, optional: 4), of: methodWrapper),
    (144, .init(stored: 96, optional: .init(72))))
  expectEqual(
    valueWithGradient(at: Class(stored: 3, optional: nil), of: methodWrapper),
    (9, .init(stored: 6, optional: .init(0))))
}

runAllTests()

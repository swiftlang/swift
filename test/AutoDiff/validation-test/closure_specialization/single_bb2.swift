/// Single basic block VJP.

// REQUIRES: executable_test

// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -o %t/none.out -Onone
// RUN: %target-build-swift %s -o %t/opt.out  -O
// RUN: %target-codesign %t/none.out
// RUN: %target-codesign %t/opt.out
// RUN: %target-run %t/none.out
// RUN: %target-run %t/opt.out

// RUN: %target-swift-frontend -emit-sil %s -O -o %t/out.sil
// RUN: cat %t/out.sil | %FileCheck %s --check-prefix=CHECK4
// RUN: cat %t/out.sil | %FileCheck %s --check-prefix=CHECK5

import DifferentiationUnittest
import StdlibUnittest

var AutoDiffClosureSpecSingleBBTests = TestSuite("AutoDiffClosureSpecSingleBB")

AutoDiffClosureSpecSingleBBTests.testWithLeakChecking("Test4") {
  func square(_ x: Float) -> Float {
    return x * x
  }

  func double(_ x: Float) -> Float {
    return x + x
  }

  // CHECK4-LABEL: {{^}}// reverse-mode derivative of test4 #1 (_:)
  // CHECK4-NEXT:  sil private @$s3outyycfU_5test4L_yS2fFTJrSpSr : $@convention(thin) (Float) -> (Float, @owned @callee_guaranteed (Float) -> Float) {
  // CHECK4:         %[[#D9:]] = function_ref @$s3outyycfU_5test4L_yS2fFTJpSpSr128$s3outyycfU_6doubleL_yS2fFTJpSpSr067$sSf16_DifferentiationE7_vjpAdd3lhs3rhsSf5value_Sf_SftSfc8pullbacktj1_k5FZSf_K6SfcfU_Tf1nc_n0c12U_6squareL_yefg7Sr073$si1_j4E12_l16Multiply3lhs3rhsn1_o1_pq1_rs1_tu2U_eV2_nS2fTf1ncc_n : $@convention(thin) (Float, Float, Float) -> Float
  // CHECK4:         %[[#D10:]] = partial_apply [callee_guaranteed] %[[#D9]](%[[#]], %[[#]]) : $@convention(thin) (Float, Float, Float) -> Float
  // CHECK4:         %[[#D11:]] = tuple (%[[#]], %[[#D10]])
  // CHECK4:         return %[[#D11]]
  // CHECK4:       } // end sil function '$s3outyycfU_5test4L_yS2fFTJrSpSr'

  // CHECK4-NONE:  {{^}}// pullback of test4 #1 (_:)
  // CHECK4:       {{^}}// specialized pullback of test4 #1 (_:)
  // CHECK4:       sil private @$s3outyycfU_5test4L_yS2fFTJpSpSr128$s3outyycfU_6doubleL_yS2fFTJpSpSr067$sSf16_DifferentiationE7_vjpAdd3lhs3rhsSf5value_Sf_SftSfc8pullbacktj1_k5FZSf_K6SfcfU_Tf1nc_n0c12U_6squareL_yefg7Sr073$si1_j4E12_l16Multiply3lhs3rhsn1_o1_pq1_rs1_tu2U_eV2_nS2fTf1ncc_n : $@convention(thin) (Float, Float, Float) -> Float {

  @differentiable(reverse)
  func test4(_ x: Float) -> Float {
    return square(double(x))
  }

  func test4Derivative(_ x: Float) -> Float {
    return 8 * x
  }

  for x in -100...100 {
    expectEqual(gradient(at: Float(x), of: test4), test4Derivative(Float(x)))
  }
}

AutoDiffClosureSpecSingleBBTests.testWithLeakChecking("Test5") {
  // CHECK5-LABEL: {{^}}// reverse-mode derivative of test5 #1 (_:_:)
  // CHECK5-NEXT:  sil private @$s3outyycfU0_5test5L_ySaySfGAC_SftFTJrSSpSr : $@convention(thin) (@guaranteed Array<Float>, Float) -> (@owned Array<Float>, @owned @callee_guaranteed (@guaranteed Array<Float>.DifferentiableView) -> (@owned Array<Float>.DifferentiableView, Float)) {
  // CHECK5:         %[[#E79:]] = function_ref @$s3outyycfU0_5test5L_ySaySfGAC_SftFTJpSSpSr073$sSf16_DifferentiationE12_vjpMultiply3lhs3rhsSf5value_Sf_SftSfc8pullbacktj1_k5FZSf_K6SfcfU_S2f0cd1_ef1_g16Subtract3lhs3rhsi1_j1_klj1_km1_kN2U_Tf1ncncn_n : $@convention(thin) (@guaranteed Array<Float>.DifferentiableView, @owned @callee_guaranteed (@guaranteed Array<Float>.DifferentiableView) -> (@owned Array<Float>.DifferentiableView, @owned Array<Float>.DifferentiableView), @owned @callee_guaranteed (@guaranteed Array<Float>.DifferentiableView) -> (@owned Array<Float>.DifferentiableView, @owned Array<Float>.DifferentiableView), Float, Float) -> (@owned Array<Float>.DifferentiableView, Float)
  // CHECK5:         %[[#E80:]] = partial_apply [callee_guaranteed] %[[#E79]](%[[#]], %[[#]], %[[#]], %[[#]]) : $@convention(thin) (@guaranteed Array<Float>.DifferentiableView, @owned @callee_guaranteed (@guaranteed Array<Float>.DifferentiableView) -> (@owned Array<Float>.DifferentiableView, @owned Array<Float>.DifferentiableView), @owned @callee_guaranteed (@guaranteed Array<Float>.DifferentiableView) -> (@owned Array<Float>.DifferentiableView, @owned Array<Float>.DifferentiableView), Float, Float) -> (@owned Array<Float>.DifferentiableView, Float)
  // CHECK5:         %[[#E81:]] = tuple (%[[#]], %[[#E80]])
  // CHECK5:         return %[[#E81]]
  // CHECK5:       } // end sil function '$s3outyycfU0_5test5L_ySaySfGAC_SftFTJrSSpSr'

  // CHECK5-NONE:  {{^}}// pullback of test5 #1 (_:_:)
  // CHECK5:       {{^}}// specialized pullback of test5 #1 (_:_:)
  // CHECK5:       sil private @$s3outyycfU0_5test5L_ySaySfGAC_SftFTJpSSpSr073$sSf16_DifferentiationE12_vjpMultiply3lhs3rhsSf5value_Sf_SftSfc8pullbacktj1_k5FZSf_K6SfcfU_S2f0cd1_ef1_g16Subtract3lhs3rhsi1_j1_klj1_km1_kN2U_Tf1ncncn_n : $@convention(thin) (@guaranteed Array<Float>.DifferentiableView, @owned @callee_guaranteed (@guaranteed Array<Float>.DifferentiableView) -> (@owned Array<Float>.DifferentiableView, @owned Array<Float>.DifferentiableView), @owned @callee_guaranteed (@guaranteed Array<Float>.DifferentiableView) -> (@owned Array<Float>.DifferentiableView, @owned Array<Float>.DifferentiableView), Float, Float) -> (@owned Array<Float>.DifferentiableView, Float) {

  @differentiable(reverse)
  func test5(_ x: [Float], _ y: Float) -> [Float] {
    return [42 * y] + x + [37 - y]
  }

  let pb = pullback(at: [Float(1), Float(1)], Float(1), of: test5)
  for a in -10...10 {
    for b in -10...10 {
      for c in -10...10 {
        for d in -10...10 {
          expectEqual(pb([Float(a), Float(b), Float(c), Float(d)]), ([Float(b), Float(c)], Float(42 * a - d)))
        }
      }
    }
  }
}

runAllTests()

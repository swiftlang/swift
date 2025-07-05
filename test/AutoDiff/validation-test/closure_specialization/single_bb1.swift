/// Single basic block VJP.

// REQUIRES: executable_test
// UNSUPPORTED: OS=windows-msvc

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

#if canImport(Glibc)
import Glibc
#elseif canImport(Android)
import Android
#else
import Foundation
#endif

var AutoDiffClosureSpecSingleBBTests = TestSuite("AutoDiffClosureSpecSingleBB")

AutoDiffClosureSpecSingleBBTests.testWithLeakChecking("Test1") {
  // CHECK1-LABEL: {{^}}// reverse-mode derivative of test1 #1 (_:)
  // CHECK1-NEXT:  sil private @$s3outyycfU_5test1L_yS2fFTJrSpSr : $@convention(thin) (Float) -> (Float, @owned @callee_guaranteed (Float) -> Float) {
  // CHECK1:         %[[#A10:]] = function_ref @$s3outyycfU_5test1L_yS2fFTJpSpSr62$s16_Differentiation7_vjpSinySf5value_S2fc8pullbacktSfFS2fcfU_Sf0c1_d1_e4Cosyg1_hiJ2U_Sf026$sSf16_DifferentiationE12_e16Multiply3lhs3rhsg1_i17_SftSfc8pullbackti1_q5FZSf_Q6SfcfU_S2fTf1nccc_n : $@convention(thin) (Float, Float, Float, Float, Float) -> Float
  // CHECK1:         %[[#A11:]] = partial_apply [callee_guaranteed] %[[#A10]](%[[#]], %[[#]], %[[#]], %[[#]]) : $@convention(thin) (Float, Float, Float, Float, Float) -> Float
  // CHECK1:         %[[#A12:]] = tuple (%[[#]], %[[#A11]])
  // CHECK1:         return %[[#A12]]
  // CHECK1:       } // end sil function '$s3outyycfU_5test1L_yS2fFTJrSpSr'

  // CHECK1-NONE:  {{^}}// pullback of test1 #1 (_:)
  // CHECK1:       {{^}}// specialized pullback of test1 #1 (_:)
  // CHECK1:       sil private @$s3outyycfU_5test1L_yS2fFTJpSpSr62$s16_Differentiation7_vjpSinySf5value_S2fc8pullbacktSfFS2fcfU_Sf0c1_d1_e4Cosyg1_hiJ2U_Sf026$sSf16_DifferentiationE12_e16Multiply3lhs3rhsg1_i17_SftSfc8pullbackti1_q5FZSf_Q6SfcfU_S2fTf1nccc_n : $@convention(thin) (Float, Float, Float, Float, Float) -> Float {

  @differentiable(reverse)
  func test1(_ x: Float) -> Float {
    return sin(x) * cos(x)
  }

  func test1Derivative(_ x: Float) -> Float {
    return cos(x) * cos(x) - sin(x) * sin(x)
  }

  for x in -100...100 {
    expectEqual((1000 * gradient(at: Float(x), of: test1)).rounded(), (1000 * test1Derivative(Float(x))).rounded())
  }
}

AutoDiffClosureSpecSingleBBTests.testWithLeakChecking("Test2") {
  // CHECK2-LABEL: {{^}}// reverse-mode derivative of test2 #1 (_:)
  // CHECK2-NEXT:  sil private @$s3outyycfU0_5test2L_yS2fFTJrSpSr : $@convention(thin) (Float) -> (Float, @owned @callee_guaranteed (Float) -> Float) {
  // CHECK2:         %[[#B19:]] = function_ref @$s3outyycfU0_5test2L_yS2fFTJpSpSr073$sSf16_DifferentiationE12_vjpMultiply3lhs3rhsSf5value_Sf_SftSfc8pullbacktj1_k5FZSf_K6SfcfU_S2f022$s16_Differentiation7_g4Sinyi15_S2fc8pullbacktJ8FS2fcfU_SfADSf0o1_p1_g4Cosyi1_rjS2U_SfACS2fAESf0cd1_e3E7_g11Add3lhs3rhsi1_j1_klj1_km1_kN2U_Tf1nccccccc_n : $@convention(thin) (Float, Float, Float, Float, Float, Float, Float, Float, Float) -> Float
  // CHECK2:         %[[#B20:]] = partial_apply [callee_guaranteed] %[[#B19]](%[[#]], %[[#]], %[[#]], %[[#]], %[[#]], %[[#]], %[[#]], %[[#]]) : $@convention(thin) (Float, Float, Float, Float, Float, Float, Float, Float, Float) -> Float
  // CHECK2:         %[[#B21:]] = tuple (%[[#]], %[[#B20]])
  // CHECK2:         return %[[#B21]]
  // CHECK2:       } // end sil function '$s3outyycfU0_5test2L_yS2fFTJrSpSr'

  // CHECK2-NONE:  {{^}}// pullback of test2 #1 (_:)
  // CHECK2:       {{^}}// specialized pullback of test2 #1 (_:)
  // CHECK2:       sil private @$s3outyycfU0_5test2L_yS2fFTJpSpSr073$sSf16_DifferentiationE12_vjpMultiply3lhs3rhsSf5value_Sf_SftSfc8pullbacktj1_k5FZSf_K6SfcfU_S2f022$s16_Differentiation7_g4Sinyi15_S2fc8pullbacktJ8FS2fcfU_SfADSf0o1_p1_g4Cosyi1_rjS2U_SfACS2fAESf0cd1_e3E7_g11Add3lhs3rhsi1_j1_klj1_km1_kN2U_Tf1nccccccc_n : $@convention(thin) (Float, Float, Float, Float, Float, Float, Float, Float, Float) -> Float {

  @differentiable(reverse)
  func test2(_ x: Float) -> Float {
    return sin(37 * x) * cos(sin(x)) + cos(x)
  }

  func test2Derivative(_ x: Float) -> Float {
    return -cos(x)*sin(37*x)*sin(sin(x)) + 37*cos(37*x)*cos(sin(x)) - sin(x)
  }

  for x in -100...100 {
    expectEqual((1000 * gradient(at: Float(x), of: test2)).rounded(), (1000 * test2Derivative(Float(x))).rounded())
  }
}

AutoDiffClosureSpecSingleBBTests.testWithLeakChecking("Test3") {
  // CHECK3-LABEL: {{^}}// reverse-mode derivative of test3 #1 (_:_:_:)
  // CHECK3-NEXT:  sil private @$s3outyycfU1_5test3L_yS2f_S2ftFTJrSSSpSr : $@convention(thin) (Float, Float, Float) -> (Float, @owned @callee_guaranteed (Float) -> (Float, Float, Float)) {
  // CHECK3:         %[[#C18:]] = function_ref @$s3outyycfU1_5test3L_yS2f_S2ftFTJpSSSpSr62$s16_Differentiation7_vjpSinySf5value_S2fc8pullbacktSfFS2fcfU_Sf0c1_d1_e4Cosyg1_hiJ2U_Sf025$sSf16_DifferentiationE7_e11Add3lhs3rhsg1_i17_SftSfc8pullbackti1_q5FZSf_Q6SfcfU_0c1_d1_e4Tanyg1_hiJ2U_Sf0lm1_n4E12_e16Subtract3lhs3rhsg1_i1_qri1_qs1_qT2U_Tf1nccccc_n : $@convention(thin) (Float, Float, Float, Float) -> (Float, Float, Float)
  // CHECK3:         %[[#C19:]] = partial_apply [callee_guaranteed] %[[#C18]](%[[#]], %[[#]], %[[#]]) : $@convention(thin) (Float, Float, Float, Float) -> (Float, Float, Float)
  // CHECK3:         %[[#C20:]] = tuple (%[[#]], %[[#C19]])
  // CHECK3:         return %[[#C20]]
  // CHECK3:       } // end sil function '$s3outyycfU1_5test3L_yS2f_S2ftFTJrSSSpSr'

  // CHECK3-NONE:  {{^}}// pullback of test3 #1 (_:_:_:)
  // CHECK3:       {{^}}// specialized pullback of test3 #1 (_:_:_:)
  // CHECK3:       sil private @$s3outyycfU1_5test3L_yS2f_S2ftFTJpSSSpSr62$s16_Differentiation7_vjpSinySf5value_S2fc8pullbacktSfFS2fcfU_Sf0c1_d1_e4Cosyg1_hiJ2U_Sf025$sSf16_DifferentiationE7_e11Add3lhs3rhsg1_i17_SftSfc8pullbackti1_q5FZSf_Q6SfcfU_0c1_d1_e4Tanyg1_hiJ2U_Sf0lm1_n4E12_e16Subtract3lhs3rhsg1_i1_qri1_qs1_qT2U_Tf1nccccc_n : $@convention(thin) (Float, Float, Float, Float) -> (Float, Float, Float) {

  @differentiable(reverse)
  func test3(_ x: Float, _ y: Float, _ z: Float) -> Float {
    return sin(x) + cos(y) - tan(z)
  }

  for x in -5...5 {
    for y in -5...5 {
      for z in -5...5 {
        let pb = pullback(at: Float(x), Float(y), Float(z), of: test3)
        let (der1, der2, der3) = pb(1)
        expectEqual((10000 * der1).rounded(), (10000 * cos(Float(x))).rounded())
        expectEqual((10000 * der2).rounded(), (10000 * (-sin(Float(y)))).rounded())
        expectEqual((10000 * der3).rounded(), (10000 * (-(tan(Float(z)) * tan(Float(z)) + 1))).rounded())
      }
    }
  }
}

runAllTests()

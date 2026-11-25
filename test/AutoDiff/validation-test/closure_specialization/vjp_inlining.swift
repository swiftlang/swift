// REQUIRES: executable_test
// RUN: %target-run-simple-swift(-parse-as-library -wmo -Xfrontend -sil-verify-all %S/loop_vjp_inlining_pipeline.swift)
// RUN: %target-run-simple-swift(-O -parse-as-library -wmo -Xfrontend -sil-verify-all %S/loop_vjp_inlining_pipeline.swift)
// RUN: %target-swift-frontend -emit-sil -O -sil-verify-all -parse-as-library -module-name src %s %S/loop_vjp_inlining_pipeline.swift -o %t.sil
// RUN: %FileCheck %s --check-prefix=DIRECT < %t.sil
// RUN: %FileCheck %s < %t.sil

import _Differentiation
import DifferentiationUnittest
import StdlibUnittest

// MARK: - Repeated VJP calls

@differentiable(reverse, wrt: x)
public func branchingStep(_ x: Float, _ flag: Bool) -> Float {
  if flag { return x * x }
  return x + x
}

// These three VJP calls exercise reuse of the same caller classification.
@differentiable(reverse, wrt: x)
public func repeatedBranchCalls(_ x: Float, _ flag: Bool) -> Float {
  var result = x
  result = branchingStep(result, flag)
  result = branchingStep(result, flag)
  result = branchingStep(result, flag)
  return result
}

// CHECK-LABEL:     // reverse-mode derivative of repeatedBranchCalls(_:_:)
// CHECK-NOT:         function_ref @$s3src13branchingStepyS2f_SbtFTJrSUpSr
// CHECK-NOT:         = partial_apply
// CHECK:             // function_ref specialized pullback of repeatedBranchCalls(_:_:)
// CHECK-NEXT:        %[[#FN:]] = function_ref @$s3src19repeatedBranchCallsyS2f_SbtFTJpSUpSr046$s3src13branchingStepyS2f_SbtFTJpSUpSr00171_38efg1_h4FTJrj16Sr$s3src047_AD__efg1_H95F_bb3__Pred__A58_0_wrt_0_spec_bb2_1_bb1_1_64fd85f0af050911ee19f0e199f1f4bdOD_BgHCDezofueTf1nb_nAA01_n2__efg1_h2F_o2__p2__a3_0_r3_0_s1_t3_1_u5_1_64V0OAcecETf1nEEE_n : $@convention(thin) (Float, _AD__$s3src13branchingStepyS2f_SbtF_bb3__Pred__src_0_wrt_0_spec_bb2_1_bb1_1_64fd85f0af050911ee19f0e199f1f4bd, _AD__$s3src13branchingStepyS2f_SbtF_bb3__Pred__src_0_wrt_0_spec_bb2_1_bb1_1_64fd85f0af050911ee19f0e199f1f4bd, _AD__$s3src13branchingStepyS2f_SbtF_bb3__Pred__src_0_wrt_0_spec_bb2_1_bb1_1_64fd85f0af050911ee19f0e199f1f4bd) -> Float
// CHECK-NEXT:        %[[#PB:]] = partial_apply [callee_guaranteed] %[[#FN]]
// CHECK-NEXT:        %[[#RESULT:]] = tuple (%[[#]], %[[#PB]])
// CHECK-NEXT:        return %[[#RESULT]]
// CHECK-NEXT:      } // end sil function

// CHECK-LABEL:     {{^}}// specialized pullback of repeatedBranchCalls(_:_:)
// CHECK:           sil private @$s3src19repeatedBranchCallsyS2f_SbtFTJpSUpSr046$s3src13branchingStepyS2f_SbtFTJpSUpSr00171_38efg1_h4FTJrj16Sr$s3src047_AD__efg1_H95F_bb3__Pred__A58_0_wrt_0_spec_bb2_1_bb1_1_64fd85f0af050911ee19f0e199f1f4bdOD_BgHCDezofueTf1nb_nAA01_n2__efg1_h2F_o2__p2__a3_0_r3_0_s1_t3_1_u5_1_64V0OAcecETf1nEEE_n :
// CHECK-COUNT-3:     switch_enum %[[#]], case #_AD__$s3src13branchingStepyS2f_SbtF_bb3__Pred__src_0_wrt_0_spec_bb2_1_bb1_1_64fd85f0af050911ee19f0e199f1f4bd.
// CHECK:           } // end sil function

public func repeatedGradient(_ x: Float, _ flag: Bool) -> Float {
  gradient(at: x, of: { repeatedBranchCalls($0, flag) })
}

// MARK: - Runtime loop tapes

public func caller(_ x: Float) -> Float {
  gradient(at: x, of: loop)
}

public func branchingCaller(_ x: Float, _ flag: Bool) -> Float {
  gradient(at: x, of: { throughBranchingLoop($0, flag) })
}

// CHECK-LABEL:     // reverse-mode derivative of branchingLoop(_:_:)
// CHECK:             // function_ref reverse-mode derivative of loop(_:)
// CHECK-NEXT:        %[[#LOOP_VJP:]] = function_ref @$s3src4loopyS2fFTJrSpSr
// CHECK-NEXT:        apply %[[#LOOP_VJP]]
// CHECK:             // function_ref specialized pullback of branchingLoop(_:_:)
// CHECK-NEXT:        %[[#BRANCH_FN:]] = function_ref @$s3src13branchingLoopyS2f_SbtFTJpSUpSr00165_38$s3src13branchingLoopyS2f_SbtFTJrSUpSr$s3src047_AD__$s3src13branchingLoopyS2f_SbtF_bb3__Pred__A52_0_wrt_0_spec_bb2_1_48d6bfdc4239101c84f59a4b321bdf8bOD_qyGFAehifoeTf1nb_n
// CHECK-NEXT:        partial_apply [callee_guaranteed] %[[#BRANCH_FN]]
// CHECK:           } // end sil function

// CHECK-LABEL:     // reverse-mode derivative of throughBranchingLoop(_:_:)
// CHECK-NOT:         function_ref @$s3src13branchingLoopyS2f_SbtFTJrSUpSr
// CHECK:             // function_ref specialized pullback of branchingLoop(_:_:)
// CHECK-NEXT:        function_ref @$s3src13branchingLoopyS2f_SbtFTJpSUpSr00165_38$s3src13branchingLoopyS2f_SbtFTJrSUpSr$s3src047_AD__$s3src13branchingLoopyS2f_SbtF_bb3__Pred__A52_0_wrt_0_spec_bb2_1_48d6bfdc4239101c84f59a4b321bdf8bOD_qyGFAehifoeTf1nb_n
// CHECK-NOT:         function_ref @$s3src13branchingLoopyS2f_SbtFTJrSUpSr
// CHECK:           } // end sil function

// CHECK-LABEL:     // reverse-mode derivative of scaleLoop(_:)
// CHECK-NOT:         function_ref @$s3src4loopyS2fFTJrSpSr
// CHECK:             %[[#SCALE_CONTEXT:]] = builtin "autoDiffCreateLinearMapContextWithType"
// CHECK-NOT:         function_ref @$s3src4loopyS2fFTJrSpSr
// CHECK:             // function_ref specialized pullback of scaleLoop(_:)
// CHECK-NEXT:        %[[#SCALE_FN:]] = function_ref @$s3src9scaleLoopyS2fFTJpSpSr073$sSf16_DifferentiationE12_vjpMultiply3lhs3rhsSf5value_Sf_SftSfc8pullbacktk1_l5FZSf_L6SfcfU_S2fTf1nnE_n23$s3src4loopyS2fFTJpSpSrBoTf1nEnn_n
// CHECK-NEXT:        partial_apply [callee_guaranteed] %[[#SCALE_FN]](%[[#]], %[[#]], %[[#SCALE_CONTEXT]]) : $@convention(thin) (Float, Float, Float, @owned Builtin.NativeObject) -> Float
// CHECK-NOT:         function_ref @$s3src4loopyS2fFTJrSpSr
// CHECK:           } // end sil function

// CHECK-LABEL:     // reverse-mode derivative of throughHalvingLoop(_:)
// CHECK-NOT:         function_ref @$s3src18forwardHalvingLoopyySfzFTJrSpSr
// CHECK:             %[[#CONTEXT:]] = builtin "autoDiffCreateLinearMapContextWithType"
// CHECK-NOT:         function_ref @$s3src18forwardHalvingLoopyySfzFTJrSpSr
// CHECK:             // function_ref specialized pullback of throughHalvingLoop(_:)
// CHECK-NEXT:        %[[#HALVE_FN:]] = function_ref @$s3src18throughHalvingLoopyS2fFTJpSpSr015$s3src18forwardC17LoopyySfzFTJpSpSrS2fIegyd_Tf1nE_n023$s3src11halvingLoopyS2fhiJ0BoTf1nE_n
// CHECK-NEXT:        %[[#HALVE_PB:]] = partial_apply [callee_guaranteed] %[[#HALVE_FN]](%[[#CONTEXT]]) : $@convention(thin) (Float, @owned Builtin.NativeObject) -> Float
// CHECK-NEXT:        %[[#HALVE_RESULT:]] = tuple (%[[#]], %[[#HALVE_PB]])
// CHECK-NEXT:        return %[[#HALVE_RESULT]]
// CHECK-NEXT:      } // end sil function

// MARK: - Direct pullback results

public protocol CoefficientProvider {
  func coefficient() -> Float
}

@differentiable(reverse, wrt: x)
public func scaledSquare(_ x: Float, _ provider: any CoefficientProvider) -> Float {
  let coefficient = provider.coefficient()
  return coefficient * x * x
}

@differentiable(reverse, wrt: x)
public func scaled(_ x: Float, _ provider: any CoefficientProvider) -> Float {
  provider.coefficient() * x
}

@differentiable(reverse, wrt: x)
public func inplaceScaledSquare(
  _ x: inout Float, _ flipSign: Bool, _ provider: any CoefficientProvider
) {
  if flipSign { x = -x }
  x = scaledSquare(x, provider)
}

@differentiable(reverse, wrt: x)
public func inplaceSignedScale(
  _ x: inout Float, _ flipSign: Bool, _ provider: any CoefficientProvider
) {
  if flipSign { x = -x }
  x = scaled(x, provider)
}

public struct Packet: Differentiable {
  public var value: Float
  @noDerivative public var label: Any

  @differentiable(reverse, wrt: value)
  public init(_ value: Float, _ label: Any) {
    self.value = value
    self.label = label
  }
}

@differentiable(reverse, wrt: x)
public func packetSignedScale(
  _ x: Float, _ flipSign: Bool, _ provider: any CoefficientProvider, _ label: Any
) -> Packet {
  let y = flipSign ? -x : x
  return Packet(scaled(y, provider), label)
}

// Inout mutations and address-only results leave the pullback as the only
// direct result.
// DIRECT-LABEL:    // reverse-mode derivative of packetSignedScale(_:_:_:_:)
// DIRECT:          sil @$s3src17packetSignedScaleyAA6PacketVSf_SbAA19CoefficientProvider_pyptFTJrSUUUpSr : $@convention(thin) (Float, Bool, @in_guaranteed any CoefficientProvider, @in_guaranteed Any) -> (@out Packet, @owned @callee_guaranteed
// DIRECT-NOT:        = partial_apply
// DIRECT:            witness_method {{.*}} #CoefficientProvider.coefficient
// DIRECT-NOT:        = partial_apply
// DIRECT:            // function_ref specialized pullback of packetSignedScale(_:_:_:_:)
// DIRECT-NEXT:       %[[#PACKET_FN:]] = function_ref @$s3src17packetSignedScaleyAA6PacketVSf_SbAA19CoefficientProvider_pyptFTJpSUUUpSr07$s3src6E22VyACSf_yptcfCTJpSUUpSrTf1nnnc_n00247_80$s3src17packetSignedScaleyAA6PacketVSf_SbAA19CoefficientProvider_pyptFTJrSUUUpSr$s3src087_AD__$s3src17packetSignedScaleyAA6PacketVSf_SbAA19CoefficientProvider_pyptF_bb3__Pred__A52_0_wrt_0_spec_bb1_1_3ceda6ed1c0661680102c3bb3878066cOD_dqDFHhimhAgTf1nbn_n022$s3src6scaledyS2f_AA19fg106_ptFTJpSUpSr073$sSf16_DifferentiationE12_vjpMultiply3lhs3rhsSf5value_Sf_SftSfc8pullbacktl1_m5FZSf_M6SfcfU_P7Tf1nE_nS2fTf1nnE_n
// DIRECT-NEXT:       %[[#PACKET_PB:]] = partial_apply [callee_guaranteed] %[[#PACKET_FN]]
// DIRECT-NEXT:       return %[[#PACKET_PB]]
// DIRECT-NEXT:     } // end sil function

// DIRECT-LABEL:    // specialized reverse-mode derivative of inplaceSignedScale(_:_:_:)
// DIRECT-NOT:        = partial_apply
// DIRECT:            witness_method {{.*}} #CoefficientProvider.coefficient
// DIRECT-NOT:        = partial_apply
// DIRECT:            // function_ref specialized pullback of inplaceSignedScale(_:_:_:)
// DIRECT-NEXT:       %[[#SCALE_FN:]] = function_ref @$s3src18inplaceSignedScaleyySfz_SbAA19CoefficientProvider_ptFTJpSUUpSr00228_70$s3src18inplaceSignedScaleyySfz_SbAA19CoefficientProvider_ptFTJrSUUpSr$s3src078_AD__$s3src18inplaceSignedScaleyySfz_SbAA19CoefficientProvider_ptF_bb3__Pred__A52_0_wrt_0_spec_bb1_1_2df9b63201c506db1db27a65dcccbe2cOD_jFGJIgvAghgTf1nbn_n022$s3src6scaledyS2f_AA19ef106_ptFTJpSUpSr073$sSf16_DifferentiationE12_vjpMultiply3lhs3rhsSf5value_Sf_SftSfc8pullbacktl1_m5FZSf_M6SfcfU_H7Tf1nE_nS2fTf1nnE_n
// DIRECT-NEXT:       %[[#SCALE_PB:]] = partial_apply [callee_guaranteed] %[[#SCALE_FN]]
// DIRECT-NEXT:       return %[[#SCALE_PB]]
// DIRECT-NEXT:     } // end sil function

// DIRECT-LABEL:    // specialized reverse-mode derivative of inplaceScaledSquare(_:_:_:)
// DIRECT-NOT:        = partial_apply
// DIRECT:            witness_method {{.*}} #CoefficientProvider.coefficient
// DIRECT-NOT:        = partial_apply
// DIRECT:            // function_ref specialized pullback of inplaceScaledSquare(_:_:_:)
// DIRECT-NEXT:       %[[#SQUARE_FN:]] = function_ref @$s3src19inplaceScaledSquareyySfz_SbAA19CoefficientProvider_ptFTJpSUUpSr00230_71$s3src19inplaceScaledSquareyySfz_SbAA19CoefficientProvider_ptFTJrSUUpSr$s3src079_AD__$s3src19inplaceScaledSquareyySfz_SbAA19CoefficientProvider_ptF_bb3__Pred__A52_0_wrt_0_spec_bb1_1_71452077f700c67476a7f18dff16b8bfOD_bcHIJgBCgjgTf1nbn_n029$s3src12scaledSquareyS2f_AA19ef106_ptFTJpSUpSr073$sSf16_DifferentiationE12_vjpMultiply3lhs3rhsSf5value_Sf_SftSfc8pullbacktm1_n5FZSf_N6SfcfU_I13ADS2fTf1nEE_nS4fTf1nnE_n
// DIRECT-NEXT:       %[[#SQUARE_PB:]] = partial_apply [callee_guaranteed] %[[#SQUARE_FN]]
// DIRECT-NEXT:       return %[[#SQUARE_PB]]
// DIRECT-NEXT:     } // end sil function

struct ConstantCoefficient: CoefficientProvider {
  let value: Float

  func coefficient() -> Float { value }
}

// MARK: - Accessor and closure primals

// The getter's derivative names its branch tracing enum after the accessor.
public struct GetterCase: Differentiable {
  public var x: Float

  public init(_ x: Float) { self.x = x }

  @differentiable(reverse)
  @inline(never)
  public var value: Float {
    if x > 0 { return x * x }
    return x + x
  }
}

public let branchClosure: @differentiable(reverse) (Float) -> Float = { x in
  if x > 0 { return x * x }
  return x + x
}

// The closure's mangling also contains the name of its enclosing function.
public func makeBranchClosure() -> @differentiable(reverse) (Float) -> Float {
  { x in
    if x > 0 { return x * x }
    return x + x
  }
}

// The global initializer's closure has no enclosing named function.

public func getterGradient(_ x: Float) -> Float {
  gradient(at: x, of: { GetterCase($0).value })
}

public func closureGradient(_ x: Float) -> Float {
  gradient(at: x, of: branchClosure)
}

// MARK: - Runtime tests

@main
struct TestDriver {
  static func main() {
    let tests = TestSuite("VJPInlining")

    tests.test("RepeatedVJPCalls") {
      for flag in [true, false] {
        let result = valueWithPullback(at: Float(2), of: {
          repeatedBranchCalls($0, flag)
        })
        let expectedDerivative: Float = flag ? 1024 : 8
        precondition(result.value == (flag ? 256 : 16))
        precondition(repeatedGradient(2, flag) == expectedDerivative)
        for seed in [Float(1), 3, -2] {
          precondition(result.pullback(seed) == expectedDerivative * seed)
        }
      }
    }

    tests.test("DirectPullbackResult") {
      for coefficient: Float in [-2, 0, 0.5] {
        let provider = ConstantCoefficient(value: coefficient)
        for flipSign in [false, true] {
          for x: Float in [-1.5, 0, 2] {
            let square = valueWithPullback(at: x) { x in
              var y = x
              inplaceScaledSquare(&y, flipSign, provider)
              return y
            }
            let signed = valueWithPullback(at: x) { x in
              var y = x
              inplaceSignedScale(&y, flipSign, provider)
              return y
            }
            let packet = valueWithPullback(at: x) {
              packetSignedScale($0, flipSign, provider, 42)
            }
            let signedCoefficient = flipSign ? -coefficient : coefficient
            precondition(square.value == coefficient * x * x)
            precondition(signed.value == signedCoefficient * x)
            precondition(packet.value.value == signed.value)
            precondition(packet.value.label as? Int == 42)
            precondition(gradient(at: x) { packetSignedScale($0, flipSign, provider, 42).value }
                         == signedCoefficient)
            for seed: Float in [1, 0, -2] {
              precondition(square.pullback(seed) == 2 * coefficient * x * seed)
              precondition(signed.pullback(seed) == signedCoefficient * seed)
              precondition(packet.pullback(.init(value: seed)) == signedCoefficient * seed)
            }
          }
        }
      }
    }

    tests.test("AccessorAndClosurePrimals") {
      for (x, expectedValue, expectedDerivative): (Float, Float, Float) in [
        (-3, -6, 2), (0, 0, 2), (3, 9, 6)
      ] {
        let getter = valueWithPullback(at: GetterCase(x), of: { $0.value })
        precondition(getter.value == expectedValue)
        precondition(getter.pullback(1).x == expectedDerivative)
        precondition(getter.pullback(3).x == 3 * expectedDerivative)
        precondition(getterGradient(x) == expectedDerivative)

        let closure = valueWithPullback(at: x, of: branchClosure)
        precondition(closure.value == expectedValue)
        precondition(closure.pullback(1) == expectedDerivative)
        precondition(closure.pullback(3) == 3 * expectedDerivative)
        precondition(closureGradient(x) == expectedDerivative)

        let nestedClosure = valueWithPullback(at: x, of: makeBranchClosure())
        precondition(nestedClosure.value == expectedValue)
        precondition(nestedClosure.pullback(1) == expectedDerivative)
        precondition(nestedClosure.pullback(3) == 3 * expectedDerivative)
      }
    }

    tests.testWithLeakChecking("RuntimeTape") {
      let cases: [(Int, Float, Float)] = [(0, 2, 1), (1, 4, 4), (3, 256, 1024)]
      for (count, value, derivative) in cases {
        iterationCount = count
        let result = valueWithPullback(at: Float(2), of: loop)
        expectEqual(value, result.value)
        expectEqual(derivative, result.pullback(1))
        expectEqual(derivative, caller(2))

        let scaled = valueWithPullback(at: Float(2), of: scaleLoop)
        expectEqual(2 * value, scaled.value)
        expectEqual(2 * derivative, scaled.pullback(1))

        let factor = Float(1) / Float(1 << count)
        let halved = valueWithPullback(at: Float(2), of: throughHalvingLoop)
        expectEqual(2 * factor, halved.value)
        for seed: Float in [1, 0, -2] {
          expectEqual(factor * seed, halved.pullback(seed))
        }

        let wrapped = valueWithPullback(at: Float(2), of: { loopOverBranch($0, false) })
        expectEqual(value, wrapped.value)
        expectEqual(derivative, wrapped.pullback(1))

        for flag in [false, true] {
          let outer = valueWithPullback(at: Float(2), of: { branchingLoop($0, flag) })
          let slope: Float = flag ? derivative : 4
          expectEqual(flag ? value : 4, outer.value)
          expectEqual(slope, outer.pullback(1))
          expectEqual(2 * slope, outer.pullback(2))
          expectEqual(slope, branchingCaller(2, flag))
        }
      }
    }

    runAllTests()
  }
}

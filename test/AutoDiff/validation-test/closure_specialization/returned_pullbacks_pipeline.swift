// RUN: %target-swift-frontend -O -sil-verify-all -emit-sil -module-name src -parse-as-library %s -o %t.sil
// RUN: %FileCheck %s < %t.sil
// RUN: %FileCheck %s --check-prefix=CONVERTED < %t.sil

import _Differentiation

// MARK: - Pullback returned directly with no direct primal result

// CHECK-LABEL:     {{^}}// reverse-mode derivative of inplaceScaledSquare
// CHECK-NOT:         partial_apply
// CHECK:             witness_method {{.*}} #CoefficientProvider.coefficient
// CHECK-NOT:         partial_apply
// CHECK:             // function_ref specialized pullback of inplaceScaledSquare
// CHECK-NEXT:        %[[#SQUARE_FN:]] = function_ref @$s3src19inplaceScaledSquareyySfz_SbAA19CoefficientProvider_ptFTJpSUUpSr00230_71$s3src19inplaceScaledSquareyySfz_SbAA19CoefficientProvider_ptFTJrSUUpSr$s3src079_AD__$s3src19inplaceScaledSquareyySfz_SbAA19CoefficientProvider_ptF_bb3__Pred__A52_0_wrt_0_spec_bb1_1_ced73ea8fed623a58e5e069e4ac79320OD_bcHIJgBCgjgTf1nbn_n029$s3src12scaledSquareyS2f_AA19ef106_ptFTJpSUpSr073$sSf16_DifferentiationE12_vjpMultiply3lhs3rhsSf5value_Sf_SftSfc8pullbacktm1_n5FZSf_N6SfcfU_I13ADS2fTf1nEE_nS4fTf1nnE_n :
// CHECK-NEXT:        %[[#SQUARE_PB:]] = partial_apply {{.*}} %[[#SQUARE_FN]](
// CHECK-NEXT:        return %[[#SQUARE_PB]]
// CHECK-NEXT:      } // end sil function
// CHECK:           {{^}}// specialized pullback of inplaceScaledSquare(_:_:_:)
// CHECK-NOT:       {{^}}sil
// CHECK:           {{^}}sil private @$s3src19inplaceScaledSquareyySfz_SbAA19CoefficientProvider_ptFTJpSUUpSr00230_71$s3src19inplaceScaledSquareyySfz_SbAA19CoefficientProvider_ptFTJrSUUpSr$s3src079_AD__$s3src19inplaceScaledSquareyySfz_SbAA19CoefficientProvider_ptF_bb3__Pred__A52_0_wrt_0_spec_bb1_1_ced73ea8fed623a58e5e069e4ac79320OD_bcHIJgBCgjgTf1nbn_n029$s3src12scaledSquareyS2f_AA19ef106_ptFTJpSUpSr073$sSf16_DifferentiationE12_vjpMultiply3lhs3rhsSf5value_Sf_SftSfc8pullbacktm1_n5FZSf_N6SfcfU_I13ADS2fTf1nEE_nS4fTf1nnE_n :
// CHECK-NOT:         partial_apply
// CHECK-NOT:         apply {{.*}} : $@callee_guaranteed
// CHECK:           } // end sil function

// CHECK-LABEL:     {{^}}// reverse-mode derivative of inplaceSignedScale
// CHECK-NOT:         partial_apply
// CHECK:             witness_method {{.*}} #CoefficientProvider.coefficient
// CHECK-NOT:         partial_apply
// CHECK:             // function_ref specialized pullback of inplaceSignedScale
// CHECK-NEXT:        %[[#SCALE_FN:]] = function_ref @$s3src18inplaceSignedScaleyySfz_SbAA19CoefficientProvider_ptFTJpSUUpSr00228_70$s3src18inplaceSignedScaleyySfz_SbAA19CoefficientProvider_ptFTJrSUUpSr$s3src078_AD__$s3src18inplaceSignedScaleyySfz_SbAA19CoefficientProvider_ptF_bb3__Pred__A52_0_wrt_0_spec_bb1_1_c32126d17ed36d7b5513667817de9994OD_jFGJIgvAghgTf1nbn_n022$s3src6scaledyS2f_AA19ef106_ptFTJpSUpSr073$sSf16_DifferentiationE12_vjpMultiply3lhs3rhsSf5value_Sf_SftSfc8pullbacktl1_m5FZSf_M6SfcfU_H7Tf1nE_nS2fTf1nnE_n :
// CHECK-NEXT:        %[[#SCALE_PB:]] = partial_apply {{.*}} %[[#SCALE_FN]](
// CHECK-NEXT:        return %[[#SCALE_PB]]
// CHECK-NEXT:      } // end sil function
// CHECK:           {{^}}// specialized pullback of inplaceSignedScale(_:_:_:)
// CHECK-NOT:       {{^}}sil
// CHECK:           {{^}}sil private @$s3src18inplaceSignedScaleyySfz_SbAA19CoefficientProvider_ptFTJpSUUpSr00228_70$s3src18inplaceSignedScaleyySfz_SbAA19CoefficientProvider_ptFTJrSUUpSr$s3src078_AD__$s3src18inplaceSignedScaleyySfz_SbAA19CoefficientProvider_ptF_bb3__Pred__A52_0_wrt_0_spec_bb1_1_c32126d17ed36d7b5513667817de9994OD_jFGJIgvAghgTf1nbn_n022$s3src6scaledyS2f_AA19ef106_ptFTJpSUpSr073$sSf16_DifferentiationE12_vjpMultiply3lhs3rhsSf5value_Sf_SftSfc8pullbacktl1_m5FZSf_M6SfcfU_H7Tf1nE_nS2fTf1nnE_n :
// CHECK-NOT:         partial_apply
// CHECK-NOT:         apply {{.*}} : $@callee_guaranteed
// CHECK:           } // end sil function

// An address-only primal result also leaves the pullback as the only direct result.
// CHECK-LABEL:     {{^}}// reverse-mode derivative of packetSignedScale
// CHECK:           sil {{.*}} -> (@out Packet, @owned @callee_guaranteed
// CHECK-NOT:         partial_apply
// CHECK:             witness_method {{.*}} #CoefficientProvider.coefficient
// CHECK-NOT:         partial_apply
// CHECK:             // function_ref specialized pullback of packetSignedScale
// CHECK-NEXT:        %[[#PACKET_FN:]] = function_ref @$s3src17packetSignedScaleyAA6PacketVSf_SbAA19CoefficientProvider_pyptFTJpSUUUpSr07$s3src6E22VyACSf_yptcfCTJpSUUpSrTf1nnnc_n00247_80$s3src17packetSignedScaleyAA6PacketVSf_SbAA19CoefficientProvider_pyptFTJrSUUUpSr$s3src087_AD__$s3src17packetSignedScaleyAA6PacketVSf_SbAA19CoefficientProvider_pyptF_bb3__Pred__A52_0_wrt_0_spec_bb1_1_8268fd98954bf992e5984bc6a71d012bOD_dqDFHhimhAgTf1nbn_n022$s3src6scaledyS2f_AA19fg106_ptFTJpSUpSr073$sSf16_DifferentiationE12_vjpMultiply3lhs3rhsSf5value_Sf_SftSfc8pullbacktl1_m5FZSf_M6SfcfU_P7Tf1nE_nS2fTf1nnE_n :
// CHECK-NEXT:        %[[#PACKET_PB:]] = partial_apply {{.*}} %[[#PACKET_FN]](
// CHECK-NEXT:        return %[[#PACKET_PB]]
// CHECK-NEXT:      } // end sil function
// CHECK:           {{^}}// specialized pullback of packetSignedScale(_:_:_:_:)
// CHECK-NOT:       {{^}}sil
// CHECK:           {{^}}sil private @$s3src17packetSignedScaleyAA6PacketVSf_SbAA19CoefficientProvider_pyptFTJpSUUUpSr07$s3src6E22VyACSf_yptcfCTJpSUUpSrTf1nnnc_n00247_80$s3src17packetSignedScaleyAA6PacketVSf_SbAA19CoefficientProvider_pyptFTJrSUUUpSr$s3src087_AD__$s3src17packetSignedScaleyAA6PacketVSf_SbAA19CoefficientProvider_pyptF_bb3__Pred__A52_0_wrt_0_spec_bb1_1_8268fd98954bf992e5984bc6a71d012bOD_dqDFHhimhAgTf1nbn_n022$s3src6scaledyS2f_AA19fg106_ptFTJpSUpSr073$sSf16_DifferentiationE12_vjpMultiply3lhs3rhsSf5value_Sf_SftSfc8pullbacktl1_m5FZSf_M6SfcfU_P7Tf1nE_nS2fTf1nnE_n :
// CHECK-NOT:         partial_apply
// CHECK-NOT:         apply {{.*}} : $@callee_guaranteed
// CHECK:           } // end sil function

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

// MARK: - Returned inout and @Sendable pullbacks

// Exercise an indirect primal result and debug uses on both sides of the
// returned pullback's @Sendable conversion.
// CONVERTED-LABEL: {{^}}// vjpMeasuredSquare(_:_:)
// CONVERTED:       sil {{.*}} -> (@out Measurement,
// CONVERTED-NOT:     partial_apply
// CONVERTED:         // function_ref specialized closure #1 in pullbackOnValue(_:)
// CONVERTED-NEXT:    %[[#MEASURED:]] = function_ref @$s3src15pullbackOnValueySfAA11MeasurementV13TangentVectorVYbcS2fYbcFSfAFYbcfU_030$s3src17vjpMeasuredSquareyAA11e14V5value_SfAD13fg12Vc8pullbacktM14_yptFS2fYbcfU_SfTf1nE_n :
// CONVERTED-NEXT:    %[[#PB:]] = partial_apply {{.*}} %[[#MEASURED]](
// CONVERTED:         %[[#CONVERTED:]] = convert_function %[[#PB]] to $@callee_guaranteed
// CONVERTED-NOT:     partial_apply
// CONVERTED:         return %[[#CONVERTED]]

// CHECK-LABEL:     {{^}}// reverse-mode derivative of branch(_:_:)
// CHECK-NOT:         partial_apply
// CHECK:             // function_ref specialized pullback of branch(_:_:)
// CHECK-NEXT:        %[[#ROOT:]] = function_ref @$s3src6branchyySfz_SbtFTJpSUpSr00151_31$s3src6branchyySfz_SbtFTJrSUpSr$s3src040_AD__$s3src6branchyySfz_SbtF_bb3__Pred__A52_0_wrt_0_spec_bb1_1_897dcaf13892f8147503c34e4ac9960fOD_ctFCEdJyeJdTf1nbn_n0102$s3src11fourthPoweryySfzFTJpSpSr85$sSf16_DifferentiationE18_vjpMultiplyAssignyyt5value_S2fzc8pullbacktE30_SftFZS2fzcfU_S2fACS2fTf1nEE_nS4fTf1nnE_n :
// CHECK-NEXT:        %[[#PB:]] = partial_apply {{.*}} %[[#ROOT]](
// CHECK-NOT:         partial_apply
// CHECK:             return %[[#PB]]
// CHECK-LABEL:     {{^}}// specialized pullback of branch(_:_:)
// CHECK-NOT:         apply {{.*}} : $@callee_guaranteed
// CHECK:           } // end sil function

@differentiable(reverse)
func fourthPower(_ x: inout Float) {
  x *= x
  x *= x
}

@differentiable(reverse, wrt: x)
public func branch(_ x: inout Float, _ square: Bool) {
  if square {
    x *= x
  }
  fourthPower(&x)
}

public struct Measurement: Differentiable {
  var value: Float
  @noDerivative var label: Any
}

@differentiable(reverse, wrt: x)
public func measuredSquare(_ x: Float, _ label: Any) -> Measurement {
  Measurement(value: x * x, label: label)
}

@inline(__always)
func pullbackOnValue(_ body: @escaping @Sendable (Float) -> Float)
  -> @Sendable (Measurement.TangentVector) -> Float {
  { seed in body(seed.value) }
}

@derivative(of: measuredSquare, wrt: x)
public func vjpMeasuredSquare(_ x: Float, _ label: Any)
  -> (value: Measurement, pullback: (Measurement.TangentVector) -> Float) {
  let body = pullbackOnValue { 2 * x * $0 }
  let callback: (Measurement.TangentVector) -> Float = body
  return (measuredSquare(x, label), callback)
}

// MARK: - Specializing leaf callbacks before inlining

// CHECK-LABEL:     {{^}}// reverse-mode derivative of branchTwoCalls(_:_:)
// CHECK-NOT:         = partial_apply
// CHECK:             // function_ref specialized pullback of branchTwoCalls(_:_:)
// CHECK-NEXT:        %[[#FN:]] = function_ref @$s3src14branchTwoCallsyySfz_SbtFTJpSUpSr00169_40$s3src14branchTwoCallsyySfz_SbtFTJrSUpSr$s3src049_AD__$s3src14branchTwoCallsyySfz_SbtF_bb3__Pred__A52_0_wrt_0_spec_bb1_1_36d82bccf45474846d27abc30aee0991OD_JJGDCetmfseTf1nbn_n041$s3src9fourStepsyySfzFTJpSpSr28$s3src8twofghi9SrSfIegl_lm5_ACSfm1_lm46_Tf1nEE_n015$s3src6squareyyfghI0S2fIegld_ADS2ft1_ut1_ut1_o94EEEE_n068$sSf16_DifferentiationE18_vjpMultiplyAssignyyt5value_S2fzc8pullbacktF14_SftFZS2fzcfU_s15AES2fAES2fAES2foV2_nS8fTf1nnE_n00206_40$s3src14branchTwoCallsyySfz_SbtFTJrSUpSr$s3src049_AD__$s3src14branchTwoCallsyySfz_SbtF_bb3__Pred__a53_0_wrt_0_spec_bb1_1_36d82bccf45474846d27abc30aee0991_K33_ce5f3ecac70b20b7c224e4f981d1c506OD_BcFAJfzogufTf1nbnnnnnnnn_n :
// CHECK-NEXT:        %[[#PB:]] = partial_apply {{.*}} %[[#FN]](
// CHECK-NEXT:        return %[[#PB]]
// CHECK-NOT:         = partial_apply
// CHECK:           } // end sil function

@differentiable(reverse)
func square(_ x: inout Float) {
  x *= x
}

@differentiable(reverse)
func twoSteps(_ x: inout Float) {
  square(&x)
  square(&x)
}

@differentiable(reverse)
func fourSteps(_ x: inout Float) {
  twoSteps(&x)
  twoSteps(&x)
}

// Specialize fourSteps' leaf callbacks before inlining its VJP into the caller.
@differentiable(reverse, wrt: x)
public func branchTwoCalls(_ x: inout Float, _ square: Bool) {
  fourSteps(&x)
  if square { x *= x }
  fourSteps(&x)
}

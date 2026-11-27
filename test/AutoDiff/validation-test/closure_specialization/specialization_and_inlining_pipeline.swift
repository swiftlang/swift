// Exercise closure specialization and derivative inlining in the optimizer pipeline.

// RUN: %target-swift-frontend -O -sil-verify-all -parse-as-library -emit-sil -module-name src %s -o - | %FileCheck %s

import _Differentiation

// MARK: - Preserving generic call-site specialization history

// CHECK-LABEL:     {{^}}// derivative(_:_:_:)
// CHECK-NOT:         function_ref {{.*}}TJr
// CHECK-NOT:         partial_apply
// CHECK:             cond_br
// CHECK-NOT:         apply
// CHECK:             builtin "fmul_FPIEEE32"
// CHECK-NOT:         apply
// CHECK:           } // end sil function

@inline(never)
@differentiable(reverse, wrt: x)
func branch<T>(_ x: Float, _ y: Float, _: T.Type, _ flag: Bool) -> Float {
  if flag { return x + y }
  return x * x
}

public func derivative(_ x: Float, _ y: Float, _ flag: Bool) -> Float {
  gradient(at: x) { branch($0, y, Int.self, flag) }
}

// MARK: - Inlining repeated VJPs

// CHECK-LABEL:     {{^}}// repeatedGradient(_:_:)
// CHECK-NOT:         function_ref @$s3src13branchingStepyS2f_SbtFTJrSUpSr :
// CHECK-NOT:         partial_apply
// CHECK:             // function_ref specialized pullback of closure #1 in repeatedGradient(_:_:)
// CHECK-NEXT:        %[[#REPEATED:]] = function_ref @$s3src16repeatedGradientyS2f_SbtFS2fcfU_TJpSUpSr44$s3src19repeatedBranchCallsyS2f_SbtFTJpSUpSrS2fIegyd_S2fIegyd_S2fIegyd_Tf1nE_n022$s3src13branchingStepyg1_hij10Sr00171_38lmg1_h4FTJrj16Sr$s3src047_AD__lmg1_H95F_bb3__Pred__A58_0_wrt_0_spec_bb2_1_bb1_1_968ae43e55d02f1c2d3fa54e10873221OD_BgHCDezofueTf1nb_nAA01_q2__lmg1_h2F_r2__s2__a3_0_u3_0_v1_w3_1_x6_1_968Y0OAdfdFTf1nEEE_n :
// CHECK:             %[[#RESULT:]] = apply %[[#REPEATED]](%[[#]], %[[#]], %[[#]], %[[#]]) : $@convention(thin) (Float, _AD__$s3src13branchingStepyS2f_SbtF_bb3__Pred__src_0_wrt_0_spec_bb2_1_bb1_1_968ae43e55d02f1c2d3fa54e10873221, _AD__$s3src13branchingStepyS2f_SbtF_bb3__Pred__src_0_wrt_0_spec_bb2_1_bb1_1_968ae43e55d02f1c2d3fa54e10873221, _AD__$s3src13branchingStepyS2f_SbtF_bb3__Pred__src_0_wrt_0_spec_bb2_1_bb1_1_968ae43e55d02f1c2d3fa54e10873221) -> Float
// CHECK-NEXT:        return %[[#RESULT]]
// CHECK-NEXT:      } // end sil function

@differentiable(reverse, wrt: x)
public func branchingStep(_ x: Float, _ flag: Bool) -> Float {
  if flag { return x * x }
  return x + x
}

// The composed pullback receives a specialized tape for each call.
@differentiable(reverse, wrt: x)
public func repeatedBranchCalls(_ x: Float, _ flag: Bool) -> Float {
  var result = x
  result = branchingStep(result, flag)
  result = branchingStep(result, flag)
  result = branchingStep(result, flag)
  return result
}

public func repeatedGradient(_ x: Float, _ flag: Bool) -> Float {
  gradient(at: x, of: { repeatedBranchCalls($0, flag) })
}

// MARK: - Inlining VJPs whose only direct result is the pullback

// CHECK-LABEL:     {{^}}// full(_:_:_:)
// CHECK-NOT:       {{^}}sil
// CHECK:           {{^}}sil [noinline] @$s3src4fullyS2f_SbAA13ScaleProvider_ptF :
// CHECK-NOT:         function_ref {{.*}}TJr
// CHECK-NOT:         function_ref {{.*}}TJp
// CHECK-NOT:         apply {{.*}} : $@callee_guaranteed
// CHECK-NOT:         partial_apply
// CHECK-COUNT-4:     witness_method {{.*}} #ScaleProvider.coefficient
// CHECK-NOT:         function_ref {{.*}}TJr
// CHECK-NOT:         function_ref {{.*}}TJp
// CHECK-NOT:         apply {{.*}} : $@callee_guaranteed
// CHECK-NOT:         partial_apply
// CHECK:           } // end sil function
// CHECK-LABEL:     {{^}}// fullWeighted(_:_:_:)
// CHECK-NOT:       {{^}}sil
// CHECK:           {{^}}sil [noinline] @$s3src12fullWeightedyS2f_SbAA13ScaleProvider_ptF :
// CHECK-NOT:         function_ref {{.*}}TJr
// CHECK-NOT:         function_ref {{.*}}TJp
// CHECK-NOT:         apply {{.*}} : $@callee_guaranteed
// CHECK-NOT:         partial_apply
// CHECK-COUNT-4:     witness_method {{.*}} #ScaleProvider.coefficient
// CHECK-NOT:         function_ref {{.*}}TJr
// CHECK-NOT:         function_ref {{.*}}TJp
// CHECK-NOT:         apply {{.*}} : $@callee_guaranteed
// CHECK-NOT:         partial_apply
// CHECK:           } // end sil function

public protocol ScaleProvider {
  func coefficient() -> Float
}

@differentiable(reverse, wrt: x)
public func leaf(_ x: Float, _ provider: any ScaleProvider) -> Float {
  let c = provider.coefficient()
  return c * x * x
}

@differentiable(reverse, wrt: x)
public func outer(_ x: inout Float, _ flag: Bool, _ provider: any ScaleProvider) {
  if flag { x = -x }
  x = leaf(x, provider)
  x = leaf(x, provider)
  x = leaf(x, provider)
  x = leaf(x, provider)
}

// Inline the outer VJP into the gradient before the four leaf VJPs to keep
// the outer VJP within budget and let the optimizer eliminate the pullback contexts.
@inline(never)
public func full(_ x: Float, _ flag: Bool, _ provider: any ScaleProvider) -> Float {
  gradient(at: x) { x in
    var y = x
    outer(&y, flag, provider)
    return y
  }
}

@differentiable(reverse, wrt: x)
public func weightedLeaf(_ x: Float, _ provider: any ScaleProvider) -> Float {
  let c = provider.coefficient()
  var y = c * x * x
  y += provider.coefficient() * x
  y += provider.coefficient() * x
  y += provider.coefficient() * x
  return y
}

@differentiable(reverse, wrt: x)
public func weighted(_ x: inout Float, _ flag: Bool, _ provider: any ScaleProvider) {
  if flag { x = -x }
  x = weightedLeaf(x, provider)
}

// Even one larger leaf VJP can exhaust the outer VJP's inlining budget here.
@inline(never)
public func fullWeighted(_ x: Float, _ flag: Bool, _ provider: any ScaleProvider) -> Float {
  gradient(at: x) { x in
    var y = x
    weighted(&y, flag, provider)
    return y
  }
}

// MARK: - Exposing nested captures after mid-level inlining

// CHECK-LABEL:     {{^}}// fullNested(_:_:_:)
// CHECK-NOT:       {{^}}sil
// CHECK:           {{^}}sil [noinline] @$s3src10fullNestedyS2f_SbAA8Provider_ptF :
// CHECK-NOT:         alloc_ref
// CHECK-NOT:         alloc_box
// CHECK-NOT:         alloc_existential_box
// CHECK-NOT:         partial_apply
// CHECK-NOT:         function_ref {{.*}}TJ{{[pr]}}
// CHECK:           } // end sil function

public protocol Provider {
  func coefficient() -> Float
}

@differentiable(reverse, wrt: x)
public func nestedLeaf(_ x: Float, _ provider: any Provider) -> Float {
  x * x * provider.coefficient()
}

@differentiable(reverse, wrt: x)
public func child(_ x: Float, _ flag: Bool, _ provider: any Provider) -> Float {
  if flag { return nestedLeaf(x, provider) }
  let y = nestedLeaf(x, provider)
  return y + y
}

@differentiable(reverse, wrt: x)
public func parent(_ x: Float, _ flag: Bool, _ provider: any Provider) -> Float {
  var x = x
  x = child(x, flag, provider)
  x = child(x, flag, provider)
  x = child(x, flag, provider)
  return x
}

// The witness call keeps the leaf VJP out of early inlining. The second
// ADCS run can specialize its captures once mid-level inlining exposes them.
@inline(never)
public func fullNested(_ x: Float, _ flag: Bool, _ provider: any Provider) -> Float {
  gradient(at: x) { parent($0, flag, provider) }
}

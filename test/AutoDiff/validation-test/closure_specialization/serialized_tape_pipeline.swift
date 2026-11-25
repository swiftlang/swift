// Specialize the tape after the optimizer has serialized the inlinable derivative.
// RUN: %target-swift-frontend -emit-sil -O -sil-verify-all -module-name src %s | %FileCheck %s

import _Differentiation

// CHECK-LABEL:     {{^}}// reverse-mode derivative of branch(_:_:)
// CHECK-NOT:       {{^sil }}
// CHECK:           {{^}}sil @$s3src6branchyS2f_SbtFTJrSUpSr :
// CHECK:             // function_ref specialized pullback of branch(_:_:)
// CHECK-NEXT:        %[[#PULLBACK:]] = function_ref @$s3src6branchyS2f_SbtFTJpSUpSr00155_30$s3src6branchyS2f_SbtFTJrSUpSr$s3src039_AD__$s3src6branchyS2f_SbtF_bb3__Pred__A58_0_wrt_0_spec_bb2_1_bb1_1_9f348be9c48d3853b71388ab1f7ebe7cOD_vEFAGdmDeeeTf1nb_n
// CHECK:             = partial_apply {{.*}} %[[#PULLBACK]](
// CHECK-LABEL:     {{^}}// specialized pullback of branch(_:_:)
// CHECK-NOT:       {{^sil }}
// CHECK:           {{^}}sil shared @$s3src6branchyS2f_SbtFTJpSUpSr00155_30$s3src6branchyS2f_SbtFTJrSUpSr$s3src039_AD__$s3src6branchyS2f_SbtF_bb3__Pred__A58_0_wrt_0_spec_bb2_1_bb1_1_9f348be9c48d3853b71388ab1f7ebe7cOD_vEFAGdmDeeeTf1nb_n :
// CHECK:             switch_enum
// CHECK-NOT:         = apply {{.*}} : $@callee_guaranteed
// CHECK:           } // end sil function

@inlinable
@differentiable(reverse, wrt: x)
public func branch(_ x: Float, _ flag: Bool) -> Float {
  if flag { return x * x }
  return x + x
}

// RUN: %target-swift-frontend -O -sil-verify-all -emit-sil -module-name src %s -o - | %FileCheck %s

import _Differentiation

// MARK: - Captures in early and late specialization

// Emit the specialized predecessors in the join's case order.
// CHECK:           enum _AD__$s3src9branchingyS2f_SbtF_bb2__Pred__src_0_wrt_0_spec_bb0_0_9df891eaf8357275f1b71d58a66c131b {
// CHECK:           enum _AD__$s3src9branchingyS2f_SbtF_bb1__Pred__src_0_wrt_0_spec_bb0_0_249e470301578846d65e5daee844b4e3 {
// CHECK-LABEL:     {{^}}enum _AD__$s3src9branchingyS2f_SbtF_bb3__Pred__src_0_wrt_0_spec_bb2_1_bb1_1_2_bef2146894ec5e4ddeeb24bd82571ab4 {
// CHECK-NEXT:        case bb2((predecessor: _AD__$s3src9branchingyS2f_SbtF_bb2__Pred__src_0_wrt_0_spec_bb0_0_9df891eaf8357275f1b71d58a66c131b,
// CHECK:             case bb1((predecessor: _AD__$s3src9branchingyS2f_SbtF_bb1__Pred__src_0_wrt_0_spec_bb0_0_249e470301578846d65e5daee844b4e3,
// CHECK-LABEL:     {{^}}}

// CHECK-LABEL:     {{^}}// reverse-mode derivative of branching(_:_:)
// CHECK-NOT:       } // end sil function
// CHECK:             // function_ref specialized pullback of branching(_:_:)
// CHECK-NEXT:        %[[#PB:]] = function_ref @$s3src9branchingyS2f_SbtFTJpSUpSr00163_33$s3src9branchingyS2f_SbtFTJrSUpSr$s3src042_AD__$s3src9branchingyS2f_SbtF_bb3__Pred__A60_0_wrt_0_spec_bb2_1_bb1_1_2_bef2146894ec5e4ddeeb24bd82571ab4OD_ysGGJdbgfmeTf1nb_n :
// CHECK-NEXT:        = partial_apply [callee_guaranteed] %[[#PB]](
// CHECK:             return
// CHECK:           } // end sil function
// CHECK-LABEL:     {{^}}// specialized pullback of branching(_:_:)
// CHECK-NOT:         = partial_apply
// CHECK-NOT:         apply {{.*}} : $@callee_guaranteed
// CHECK:           } // end sil function

// Aggregate unwrapping projects payload fields across branches before ADCS.
@inline(never)
@differentiable(reverse, wrt: x)
public func branching(_ x: Float, _ flag: Bool) -> Float {
  var result = x * x
  if flag {
    result = result + result
    result = result - x
  } else {
    result = result / x
  }
  return result
}

// CHECK-LABEL:     {{^}}// reverse-mode derivative of nested(_:_:)
// CHECK-NOT:       } // end sil function
// CHECK:             // function_ref specialized pullback of nested(_:_:)
// CHECK-NEXT:        %[[#PB:]] = function_ref @$s3src6nestedyS2f_SftFTJpSSpSr00170_30$s3src6nestedyS2f_SftFTJrSSpSr$s3src040_AD__$s3src6nestedyS2f_SftF_bb10__Pred__A72_0_wrt_0_1_spec_bb9_1_bb6_1_bb5_1_bb3_1_0993556bd9650eb45ab1877d007b08ffOD_FdCICewnfteTf1nb_n :
// CHECK-NEXT:        = partial_apply [callee_guaranteed] %[[#PB]](
// CHECK:             return
// CHECK:           } // end sil function
// CHECK-LABEL:     {{^}}// specialized pullback of nested(_:_:)
// CHECK-NOT:         = partial_apply
// CHECK-NOT:         apply {{.*}} : $@callee_guaranteed
// CHECK:           } // end sil function

@inline(never)
@differentiable(reverse)
public func nested(_ x: Float, _ y: Float) -> Float {
  if x > 0 {
    if y > 10 {
      let z = x * y
      if z > 100 { return x + z }
      if y == 20 { return z + z }
    } else {
      return x + y
    }
  }
  return -y
}

// CHECK-LABEL:     {{^}}// reverse-mode derivative of generic<A>(_:_:)
// CHECK-NOT:       } // end sil function
// CHECK:             // function_ref specialized pullback of generic<A>(_:_:)
// CHECK-NEXT:        %[[#PB:]] = function_ref @$s3src7genericyS2f_xmtlFlTJpSUpSr00165_33$s3src7genericyS2f_xmtlFlTJrSUpSr$s3src041_AD__$s3src7genericyS2f_xmtlF_bb6__Pred__A60_0_wrt_0_l_spec_bb5_1_bb4_1_b59882e3d738abc015825ee7333787faOyxGD_qyGFAehifoeTf1nb_n :
// CHECK-NEXT:        = partial_apply [callee_guaranteed] %[[#PB]]<
// CHECK:             return
// CHECK:           } // end sil function
// CHECK-LABEL:     {{^}}// specialized pullback of generic<A>(_:_:)
// CHECK-NOT:         = partial_apply
// CHECK-NOT:         apply {{.*}} : $@callee_guaranteed
// CHECK:           } // end sil function

@inline(never)
@differentiable(reverse, wrt: x)
public func generic<T>(_ x: Float, _ metatype: T.Type) -> Float {
  if metatype is Int.Type { return x + x }
  return x * x
}

@inline(never)
@differentiable(reverse)
public func delayedSquare(_ x: Float) -> Float { x * x }

// Delay exposing the callback until mid-level inlining, after the first ADCS run.
@_semantics("inline_late")
@derivative(of: delayedSquare)
public func delayedSquareVJP(_ x: Float) -> (value: Float, pullback: (Float) -> Float) {
  (x * x, { seed in 2 * x * seed })
}

// CHECK-LABEL:     {{^}}// reverse-mode derivative of lateBranch(_:_:)
// CHECK-NOT:       } // end sil function
// CHECK:             // function_ref specialized pullback of lateBranch(_:_:)
// CHECK-NEXT:        %[[#PB:]] = function_ref @$s3src10lateBranchyS2f_SbtFTJpSUpSr00153_35$s3src10lateBranchyS2f_SbtFTJrSUpSr$s3src044_AD__$s3src10lateBranchyS2f_SbtF_bb3__Pred__A46_0_wrt_0_spec_968c4923c743a4157deb1f09a2999bfdOD_DyFBFdgBeceTf1nb_n :
// CHECK-NEXT:        = partial_apply [callee_guaranteed] %[[#PB]](
// CHECK:             return
// CHECK:           } // end sil function
// CHECK-LABEL:     {{^}}// specialized pullback of lateBranch(_:_:)
// CHECK-NOT:         = partial_apply
// CHECK-NOT:         apply {{.*}} : $@callee_guaranteed
// CHECK:           } // end sil function

@inline(never)
@differentiable(reverse, wrt: x)
public func lateBranch(_ x: Float, _ flag: Bool) -> Float {
  let result = delayedSquare(x)
  if flag { return result }
  return x
}

// The identity and constant paths keep a live trivial predecessor payload
// that uses tuple_extract before ADCS.
// CHECK-LABEL:     {{^}}// reverse-mode derivative of lateIdentity(_:_:_:)
// CHECK-NOT:       } // end sil function
// CHECK:             // function_ref specialized pullback of lateIdentity(_:_:_:)
// CHECK-NEXT:        %[[#FN:]] = function_ref @$s3src12lateIdentityyS2f_S2btFTJpSUUpSr00166_39$s3src12lateIdentityyS2f_S2btFTJrSUUpSr$s3src047_AD__$s3src12lateIdentityyS2f_S2btF_bb6__Pred__A52_0_wrt_0_spec_bb5_1_985f1e1b1b2f9a2b86399912312e2796OD_mBBABekjfpeTf1nb_n :
// CHECK-NEXT:        %[[#PB:]] = partial_apply [callee_guaranteed] %[[#FN]]
// CHECK-NEXT:        %[[#RESULT:]] = tuple (%[[#]], %[[#PB]])
// CHECK-NEXT:        return %[[#RESULT]]
// CHECK-NEXT:      } // end sil function

// CHECK-LABEL:     {{^}}// specialized pullback of lateIdentity(_:_:_:)
// CHECK-NOT:         apply {{.*}} : $@callee_guaranteed
// CHECK:             switch_enum %[[#]], case #_AD__$s3src12lateIdentityyS2f_S2btF_bb6__Pred__src_0_wrt_0_spec_bb5_1_985f1e1b1b2f9a2b86399912312e2796.bb5!enumelt: [[CAPTURED_CASE:bb[0-9]+]], case #_AD__$s3src12lateIdentityyS2f_S2btF_bb6__Pred__src_0_wrt_0_spec_bb5_1_985f1e1b1b2f9a2b86399912312e2796.bb4!enumelt: [[WHOLE_CASE:bb[0-9]+]]
// CHECK-NOT:         apply {{.*}} : $@callee_guaranteed
// CHECK:           [[CAPTURED_CASE]](%[[#CAPTURED_PAYLOAD:]] : $(predecessor: _AD__$s3src12lateIdentityyS2f_S2btF_bb5__Pred__src_0_wrt_0, (_: Float))):
// CHECK-NEXT:        %[[#CAPTURES:]] = tuple_extract %[[#CAPTURED_PAYLOAD]], 1
// CHECK:             %[[#CAPTURE:]] = tuple_extract %[[#CAPTURES]], 0
// CHECK-NOT:         apply {{.*}} : $@callee_guaranteed
// CHECK:           [[WHOLE_CASE]](%[[#WHOLE:]] : $(predecessor: _AD__$s3src12lateIdentityyS2f_S2btF_bb4__Pred__src_0_wrt_0)):
// CHECK-NEXT:        %[[#PRED:]] = tuple_extract %[[#WHOLE]], 0
// CHECK:             switch_enum %[[#PRED]], case #_AD__$s3src12lateIdentityyS2f_S2btF_bb4__Pred__src_0_wrt_0.bb3!enumelt
// CHECK-NOT:         apply {{.*}} : $@callee_guaranteed
// CHECK:           } // end sil function

@inline(never)
@differentiable(reverse, wrt: x)
public func lateIdentity(_ x: Float, _ first: Bool, _ second: Bool) -> Float {
  let result: Float
  if first {
    if second { result = x } else { result = 3 }
  } else {
    result = delayedSquare(x)
  }
  return result
}

@inline(never)
@differentiable(reverse, wrt: x)
public func mixedCallbacks(
  _ x: Float, _ useTransform: Bool,
  _ transform: @differentiable(reverse) (Float) -> Float
) -> Float {
  if useTransform { return transform(x) }
  return x * x
}

public func branchingGradient(_ x: Float, _ flag: Bool) -> Float {
  gradient(at: x, of: { branching($0, flag) })
}
public func nestedGradient(_ x: Float, _ y: Float) -> (Float, Float) {
  gradient(at: x, y, of: nested)
}
public func genericGradient<T>(_ x: Float, _ metatype: T.Type) -> Float {
  gradient(at: x, of: { generic($0, metatype) })
}

public func lateGradient(_ x: Float, _ flag: Bool) -> Float {
  gradient(at: x, of: { lateBranch($0, flag) })
}

public func lateIdentityGradient(_ x: Float, _ first: Bool, _ second: Bool) -> Float {
  gradient(at: x, of: { lateIdentity($0, first, second) })
}

// MARK: - Duplicate construction of one enum case

// CHECK-LABEL:     {{^}}// reverse-mode derivative of twoSelections
// CHECK-NOT:       } // end sil function
// CHECK:             // function_ref specialized pullback of twoSelections
// CHECK-NEXT:        %[[#PB:]] = function_ref @$s3src13twoSelectionsyS2f_SfSgACtFTJpSSSpSr00173_43$s3src13twoSelectionsyS2f_SfSgACtFTJrSSSpSr$s3src052_AD__$s3src13twoSelectionsyS2f_SfSgACtF_bb11__Pred__A50_0_wrt_0_1_2_spec_949c599b273d0b28d88ae64530ce5f2cOD_tmHBEeFqfweTf1nb_n :
// CHECK-NEXT:        %[[#PULLBACK:]] = partial_apply {{.*}} %[[#PB]]{{[<(]}}
// CHECK-NEXT:        %[[#RESULT:]] = tuple (%[[#]], %[[#PULLBACK]])
// CHECK-NOT:       } // end sil function
// CHECK:             return %[[#RESULT]]
// CHECK-NEXT:      } // end sil function

// CHECK-LABEL:     {{^}}// specialized pullback of twoSelections
// CHECK-NOT:         = partial_apply
// CHECK-NOT:         apply {{.*}} : $@callee_guaranteed
// CHECK:           } // end sil function

// CFG duplication creates separate tuples for the same case with identical
// payload operands.

@differentiable(reverse)
public func twoSelections(_ x: Float, _ first: Float?, _ second: Float?) -> Float {
  let square = x * x
  let intermediate: Float
  if let value = first {
    if value < square { intermediate = value }
    else { intermediate = square }
  } else { intermediate = square }
  if let value = second {
    if value < intermediate { return value }
    return intermediate
  }
  return intermediate
}

// CHECK-LABEL:     {{^}}// reverse-mode derivative of squaredSelection
// CHECK-NOT:         = partial_apply
// CHECK:             // function_ref specialized pullback of squaredSelection(_:_:_:)
// CHECK-NEXT:        %[[#SQUARE_FN:]] = function_ref @$s3src16squaredSelectionyS2f_SfSgACtFTJpSSSpSr00179_46$s3src16squaredSelectionyS2f_SfSgACtFTJrSSSpSr$s3src055_AD__$s3src16squaredSelectionyS2f_SfSgACtF_bb11__Pred__A50_0_wrt_0_1_2_spec_25d61d5855846c07d8ae563784968febOD_EDHIGeoxfCeTf1nb_n00216_46$s3src16squaredSelectionyS2f_SfSgACtFTJrSSSpSr$s3src055_AD__$s3src16squaredSelectionyS2f_SfSgACtF_bb11__Pred__a51_0_wrt_0_1_2_spec_25d61d5855846c07d8ae563784968feb_L33_7041d5535097f1de6041b2336babff7eOD_wFFFDgungEfTf1nb_n :
// CHECK-NEXT:        %[[#SQUARE_CLOSURE:]] = partial_apply {{.*}} %[[#SQUARE_FN]](
// CHECK-NEXT:        %[[#SQUARE_RESULT:]] = tuple (%[[#]], %[[#SQUARE_CLOSURE]])
// CHECK-NEXT:        return %[[#SQUARE_RESULT]]
// CHECK:           {{^}}// specialized pullback of squaredSelection(_:_:_:)
// CHECK-NOT:       {{^}}sil
// CHECK:           {{^}}sil private @$s3src16squaredSelectionyS2f_SfSgACtFTJpSSSpSr00179_46$s3src16squaredSelectionyS2f_SfSgACtFTJrSSSpSr$s3src055_AD__$s3src16squaredSelectionyS2f_SfSgACtF_bb11__Pred__A50_0_wrt_0_1_2_spec_25d61d5855846c07d8ae563784968febOD_EDHIGeoxfCeTf1nb_n00216_46$s3src16squaredSelectionyS2f_SfSgACtFTJrSSSpSr$s3src055_AD__$s3src16squaredSelectionyS2f_SfSgACtF_bb11__Pred__a51_0_wrt_0_1_2_spec_25d61d5855846c07d8ae563784968feb_L33_7041d5535097f1de6041b2336babff7eOD_wFFFDgungEfTf1nb_n :
// CHECK-NOT:         = partial_apply
// CHECK-NOT:         apply {{.*}} : $@callee_guaranteed
// CHECK:           } // end sil function

// After the first ADCS visit specializes x * x, sinking duplicates its
// capture tuple. Late inlining exposes delayedSquare's separate callback.
@differentiable(reverse)
public func squaredSelection(_ x: Float, _ first: Float?, _ second: Float?) -> Float {
  let square = x * x
  var intermediate = square
  if let value = first {
    if value < square { intermediate = value }
  }
  let shifted = delayedSquare(intermediate)
  if let value = second {
    if value < shifted { return value }
    return shifted
  }
  return shifted
}

@inline(never)
@differentiable(reverse, wrt: x)
public func maybeSquare(_ x: Float, _ square: Bool) -> Float { square ? x * x : x }

@inline(__always)
@derivative(of: maybeSquare, wrt: x)
public func maybeSquareVJP(_ x: Float, _ square: Bool) -> (value: Float, pullback: (Float) -> Float) {
  if square { return (x * x, { seed in 2 * x * seed }) }
  return (x, { seed in seed })
}

@inline(__always)
func either(_ lhs: Bool, _ rhs: Bool) -> Bool { lhs || rhs }

// The constructors agree on the delayedSquare capture but hold different
// callbacks in the neighboring field.

// CHECK-LABEL:     {{^}}// reverse-mode derivative of mixedConstructors
// CHECK:           bb0(%[[#X:]] : $Float,
// CHECK:             %[[#IDENTITY:]] = thin_to_thick_function
// CHECK:             %[[#CAP1:]] = tuple (%[[#X]])
// CHECK-NEXT:        %[[#PAY1:]] = tuple (%[[#CAP1]], %[[#IDENTITY]])
// CHECK:             // function_ref closure #1 in maybeSquareVJP(_:_:)
// CHECK-NEXT:        %[[#QUADRATIC_FN:]] = function_ref @$s3src14maybeSquareVJPySf5value_S2fc8pullbacktSf_SbtFS2fcfU_ :
// CHECK-NEXT:        %[[#QUADRATIC:]] = partial_apply [callee_guaranteed] %[[#QUADRATIC_FN]](
// CHECK:             %[[#CAP2:]] = tuple (%[[#X]])
// CHECK-NEXT:        %[[#PAY2:]] = tuple (%[[#CAP2]], %[[#QUADRATIC]])
// CHECK:             = enum $_AD__$s3src17mixedConstructorsyS2f_S2btF_bb1__Pred__src_0_wrt_0_spec_bb0_0_4ca6b35ca9fb5774fecab4d2738d8422, #_AD__$s3src17mixedConstructorsyS2f_S2btF_bb1__Pred__src_0_wrt_0_spec_bb0_0_4ca6b35ca9fb5774fecab4d2738d8422.bb0!enumelt, %[[#PAY2]]
// CHECK:             = enum $_AD__$s3src17mixedConstructorsyS2f_S2btF_bb1__Pred__src_0_wrt_0_spec_bb0_0_4ca6b35ca9fb5774fecab4d2738d8422, #_AD__$s3src17mixedConstructorsyS2f_S2btF_bb1__Pred__src_0_wrt_0_spec_bb0_0_4ca6b35ca9fb5774fecab4d2738d8422.bb0!enumelt, %[[#PAY1]]
// CHECK-LABEL:     {{^}}// specialized pullback of mixedConstructors
// CHECK:             apply {{.*}} : $@callee_guaranteed (Float) -> Float
// CHECK-NOT:         apply {{.*}} : $@callee_guaranteed
// CHECK:           } // end sil function

@differentiable(reverse, wrt: x)
public func mixedConstructors(_ x: Float, _ negate: Bool, _ squareAgain: Bool) -> Float {
  let square = delayedSquare(x)
  let value = maybeSquare(square, squareAgain)
  if either(squareAgain, negate) {
    if squareAgain { return value }
    return -value
  }
  return 0
}

// MARK: - Joining payloads from different predecessor enums

// CHECK-LABEL:     {{^}}// reverse-mode derivative of mergedPayload(_:_:)
// CHECK-NOT:       } // end sil function
// CHECK:             // function_ref specialized pullback of mergedPayload(_:_:)
// CHECK-NEXT:        %[[#PULLBACK:]] = function_ref @$s3src13mergedPayloadyS2f_SftFTJpSSpSr00173_38$s3src13mergedPayloadyS2f_SftFTJrSSpSr$s3src047_AD__$s3src13mergedPayloadyS2f_SftF_bb6__Pred__A60_0_wrt_0_1_spec_bb5_1_bb4_1_c656d0b21117ca2743578e552f4afa20OD_tmHBEeFqfweTf1nb_n :
// CHECK-NEXT:        %[[#BOUND:]] = partial_apply {{.*}} %[[#PULLBACK]](
// CHECK-NEXT:        %[[#RESULT:]] = tuple (%[[#]], %[[#BOUND]])
// CHECK-NEXT:        return %[[#RESULT]]
// CHECK-NEXT:      } // end sil function
// CHECK-LABEL:     {{^}}// specialized pullback of mergedPayload(_:_:)
// CHECK-NOT:         @callee_guaranteed
// CHECK:           } // end sil function

// Both paths save the shared block's payload in distinct predecessor enums.
// The pullback joins the whole payload before applying its closures.
@differentiable(reverse)
public func mergedPayload(_ x: Float, _ y: Float) -> Float {
  let sum = x + y
  let difference = x - y
  let selected: Float
  if x < y {
    selected = sum + y
  } else {
    selected = difference - y
  }
  let shifted = selected + x
  return x > 0 ? shifted * x : shifted + y
}

// RUN: %target-swift-frontend -O -sil-verify-all -emit-sil -module-name src %s -o - | %FileCheck %s

import _Differentiation

// MARK: - Specializing the payload of Optional.some

// Both paths retain their specialized payload type in the optimized module.
// CHECK-LABEL:     enum _AD__$s3src13tupleRecoveryySf_SftSf_SfSbtF_bb5__Pred__src_0_1_wrt_0_1_spec_bb1_1_4cd0e7fb03b07613e051f5e19ffea9e6 {
// CHECK-NEXT:        case bb1((predecessor: _AD__$s3src13tupleRecoveryySf_SftSf_SfSbtF_bb1__Pred__src_0_1_wrt_0_1, (_: Float)?))
// CHECK-LABEL:     enum _AD__$s3src13tupleRecoveryySf_SftSf_SfSbtF_bb2__Pred__src_0_1_wrt_0_1_spec_bb1_1_459bcfa9e742d74b195169789246c3f1 {
// CHECK-NEXT:        case bb1((predecessor: _AD__$s3src13tupleRecoveryySf_SftSf_SfSbtF_bb1__Pred__src_0_1_wrt_0_1, (_: Float)?))

// Both error paths use the same specialized predecessor type.
// CHECK-LABEL:     enum _AD__$s3src11recoverPairySf_SftSf_SfS2btF_bb3__Pred__src_0_1_wrt_0_1_spec_bb0_0_c42a13220250b072f90d525d467464e0 {
// CHECK-NEXT:        case bb0((_: (_: Float)?))
// CHECK-LABEL:     enum _AD__$s3src11recoverPairySf_SftSf_SfS2btF_bb5__Pred__src_0_1_wrt_0_1_spec_bb3_1_6a0908910b033d84faf71f5618612f47 {
// CHECK-NEXT:        case bb3((predecessor: _AD__$s3src11recoverPairySf_SftSf_SfS2btF_bb3__Pred__src_0_1_wrt_0_1_spec_bb0_0_c42a13220250b072f90d525d467464e0, (_: Float)?))

// The .some and .none payloads reach the same pullback argument. Its live
// optional stays intact when ADCS specializes the multiplication callback.

public enum Negative: Error {
  case input
}

@differentiable(reverse)
public func positiveSquare(_ x: Float) throws -> Float {
  if x < 0 { throw Negative.input }
  return x * x
}

@derivative(of: positiveSquare)
@inline(__always)
public func positiveSquareVJP(_ x: Float) throws -> (value: Float, pullback: (Float) -> Float) {
  if x < 0 { throw Negative.input }
  return (x * x, { 2 * x * $0 })
}

// CHECK-LABEL:     {{^}}// reverse-mode derivative of recoveringSquare(_:_:)
// CHECK:             enum $Optional<@callee_guaranteed (Float) -> Float>, #Optional.none!enumelt
// CHECK:             // function_ref closure #1 in positiveSquareVJP(_:)
// CHECK-NEXT:        %[[#ROOT:]] = function_ref @$s3src17positiveSquareVJPySf5value_S2fc8pullbacktSfKFS2fcfU_ :
// CHECK-NEXT:        %[[#CLOSURE:]] = partial_apply [callee_guaranteed] %[[#ROOT]](
// CHECK-NEXT:        enum $Optional<@callee_guaranteed (Float) -> Float>, #Optional.some!enumelt, %[[#CLOSURE]]
// CHECK:             // function_ref specialized pullback of recoveringSquare(_:_:)
// CHECK-NEXT:        %[[#PULLBACK:]] = function_ref @$s3src16recoveringSquareyS2f_SbtFTJpSUpSr00171_41$s3src16recoveringSquareyS2f_SbtFTJrSUpSr$s3src050_AD__$s3src16recoveringSquareyS2f_SbtF_bb4__Pred__A52_0_wrt_0_spec_bb3_1_d7861f59b5c2b0768ff6075bbfc9fb60OD_BgHCDezofueTf1nb_n :
// CHECK-NEXT:        partial_apply [callee_guaranteed] %[[#PULLBACK]](
// CHECK:           } // end sil function

// CHECK-LABEL:     {{^}}// specialized pullback of recoveringSquare(_:_:)
// CHECK:             builtin "fmul_FPIEEE32"
// CHECK:             switch_enum %[[#]], case #Optional.some!enumelt: [[SOME:bb[0-9]+]], case #Optional.none!enumelt:
// CHECK:           [[SOME]](%[[#CALLBACK:]] : $@callee_guaranteed (Float) -> Float):
// CHECK-NEXT:        apply %[[#CALLBACK]](
// CHECK:           } // end sil function

@differentiable(reverse, wrt: x)
public func recoveringSquare(_ x: Float, _ flag: Bool) -> Float {
  if flag {
    do { return try positiveSquare(x) }
    catch { return x }
  }
  return x * x
}

// Selecting the second result leaves the pullback with only a tape destroy.
// CHECK-LABEL:     {{^}}// reverse-mode derivative of tupleRecovery(_:_:_:)
// CHECK-NOT:         partial_apply
// CHECK:             // function_ref specialized pullback of tupleRecovery(_:_:_:)
// CHECK-NEXT:        %[[#TUPLE_FN:]] = function_ref @$s3src13tupleRecoveryySf_SftSf_SfSbtFTJpSSUpSSr00186_47$s3src13tupleRecoveryySf_SftSf_SfSbtFTJrSSUpSSr$s3src033_AD__$s3src13tupleRecoveryySf_Sfte1_e16SbtF_bb4__Pred__A56_0_1_wrt_0_1_spec_bb3_1_feb75369484c166c5b1e0fda63e17072OD_cpDAAfJEfJeTf1nnb_nTf4nnd_n : $@convention(thin) (Float, Float) -> (Float, Float)
// CHECK-NEXT:        thin_to_thick_function %[[#TUPLE_FN]]
// CHECK:           } // end sil function
@differentiable(reverse, wrt: (x, y))
@inline(never)
public func tupleRecovery(_ x: Float, _ y: Float, _ flag: Bool) -> (Float, Float) {
  if flag {
    do { return (try positiveSquare(y), 1) }
    catch { return (x, 1) }
  }
  return (y * y, 6)
}

@differentiable(reverse, wrt: (x, y))
@inline(never)
public func recoverySecond(_ x: Float, _ y: Float, _ flag: Bool) -> Float {
  tupleRecovery(x, y, flag).1
}

// MARK: - Rewriting duplicated Optional.none constructors

public enum Failure: Error { case first, second }

@differentiable(reverse, wrt: x)
public func square(_ x: Float, _ flag: Bool) throws -> Float {
  if x < 0 { throw Failure.first }
  if flag { throw Failure.second }
  return x * x
}

@derivative(of: square, wrt: x)
@inline(__always)
public func squareVJP(_ x: Float, _ flag: Bool) throws
  -> (value: Float, pullback: (Float) -> Float) {
  if x < 0 { throw Failure.first }
  if flag { throw Failure.second }
  return (x * x, squarePullback(x))
}

// Expose the callback after jump threading has duplicated the error edge.
@_semantics("inline_late")
func squarePullback(_ x: Float) -> (Float) -> Float { { 2 * x * $0 } }

// CSE and jump threading leave two error constructors with the same case and
// operands, including the first error's dominating Optional.none.
@differentiable(reverse, wrt: (x, y))
@inline(never)
public func recover(_ x: Float, _ y: Float, _ a: Bool, _ b: Bool) -> Float {
  do { return try square(y, a) }
  catch {
    do { return try square(x, b) }
    catch { return x < 0 ? y : x }
  }
}

// Taking the constant component gives a pullback that only destroys its tape.
// CHECK-LABEL:     {{^}}// reverse-mode derivative of recoverPair(_:_:_:_:)
// CHECK-NOT:         partial_apply
// CHECK:             // function_ref specialized pullback of recoverPair(_:_:_:_:)
// CHECK-NEXT:        %[[#PAIR_FN:]] = function_ref @$s3src11recoverPairySf_SftSf_SfS2btFTJpSSUUpSSr00179_47$s3src11recoverPairySf_SftSf_SfS2btFTJrSSUUpSSr$s3src031_AD__$s3src11recoverPairySf_Sfte1_e17S2btF_bb2__Pred__A50_0_1_wrt_0_1_spec_842d019bea47ccc375d8dcc81ac65852OD_EDHIGeoxfCeTf1nnb_nTf4nnd_n : $@convention(thin) (Float, Float) -> (Float, Float)
// CHECK-NEXT:        thin_to_thick_function %[[#PAIR_FN]]
// CHECK:           } // end sil function

@differentiable(reverse, wrt: (x, y))
@inline(never)
public func recoverPair(_ x: Float, _ y: Float, _ a: Bool, _ b: Bool) -> (Float, Float) {
  do { return (try square(y, a), 1) }
  catch {
    do { return (try square(x, b), 2) }
    catch { return (x < 0 ? y : x, 3) }
  }
}

@differentiable(reverse, wrt: (x, y))
public func recoveryTag(_ x: Float, _ y: Float, _ a: Bool, _ b: Bool) -> Float {
  recoverPair(x, y, a, b).1
}

// MARK: - Eliminating a tape that the pullback only destroys

// CHECK-LABEL:     {{^}}// specialized pullback of tupleBranch(_:_:)
// CHECK-NOT:       {{^}}sil
// CHECK:           sil private [signature_optimized_thunk] [heuristic_always_inline] @$s3src11tupleBranchySf_SftSf_SftFTJpSSpSSr00188_42$s3src11tupleBranchySf_SftSf_SftFTJrSSpSSr$s3src031_AD__$s3src11tupleBranchySf_Sfte1_f13F_bb7__Pred__A68_0_1_wrt_0_1_spec_bb5_1_2_3_bb2_1_2_4a23946d921387d0bfd32026c4d9ca42OD_DuDJAfgHfcfTf1nnb_nTf4nnd_n : $@convention(thin) (Float, Float) -> (Float, Float) {
// CHECK-NOT:         destroy_value
// CHECK:             return
// CHECK-NEXT:      } // end sil function

// The second component's pullback only destroys its tape. Replacing the
// saved functions with Float captures makes that tape trivial.
@differentiable(reverse)
@inline(never)
public func tupleBranch(_ x: Float, _ y: Float) -> (Float, Float) {
  if x < 0 {
    if y > 0 { return (3 * y * y, 1) }
    return (0, 1)
  }
  if y > 0 { return (4 * y * y * y, 6) }
  return (0, 6)
}

@differentiable(reverse)
@inline(never)
public func secondComponent(_ x: Float, _ y: Float) -> Float {
  tupleBranch(x, y).1
}

// This primal supplied the input for the isolated SIL test.
@differentiable(reverse, wrt: x)
public func multiplyOptional(_ x: Float, _ y: Float?) -> Float {
  if let y { return x * y }
  return x * x
}

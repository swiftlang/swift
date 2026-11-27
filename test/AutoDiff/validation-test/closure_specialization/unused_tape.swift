// REQUIRES: executable_test
// RUN: %target-run-simple-swift(-Xfrontend -sil-verify-all)
// RUN: %target-run-simple-swift(-O -Xfrontend -sil-verify-all)
// RUN: %target-swift-frontend -emit-sil -O -sil-verify-all -module-name src %s -o %t.sil
// RUN: %FileCheck %s --check-prefix=OPTIONAL < %t.sil
// RUN: %FileCheck %s < %t.sil

import _Differentiation
import DifferentiationUnittest
import StdlibUnittest

var tests = TestSuite("UnusedTape")

// MARK: - Optional callback payloads

// OPTIONAL-LABEL:  enum _AD__$s3src13tupleRecoveryySf_SftSf_SfSbtF_bb5__Pred__src_0_1_wrt_0_1_spec_bb1_1_fef93374c1dda59fc2fff98f5497bba3 {
// OPTIONAL-NEXT:     case bb1((predecessor: _AD__$s3src13tupleRecoveryySf_SftSf_SfSbtF_bb1__Pred__src_0_1_wrt_0_1, (_: Float)?))

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

// OPTIONAL-LABEL:  // specialized pullback of recoveringSquare(_:_:)
// OPTIONAL:        bb[[#]](%[[#]] : $Float, %[[#SAVED:]] : $Optional<@callee_guaranteed (Float) -> Float>):
// OPTIONAL-NEXT:     switch_enum %[[#SAVED]], case #Optional.some!enumelt: bb[[#SOME:]], case #Optional.none!enumelt:
// OPTIONAL:        bb[[#SOME]](%[[#CALLBACK:]] : $@callee_guaranteed (Float) -> Float):
// OPTIONAL-NEXT:     %[[#]] = apply %[[#CALLBACK]](%[[#]]) : $@callee_guaranteed (Float) -> Float

@differentiable(reverse, wrt: x)
public func recoveringSquare(_ x: Float, _ flag: Bool) -> Float {
  if flag {
    do { return try positiveSquare(x) }
    catch { return x }
  }
  return x * x
}

// Selecting the constant second result leaves a pullback whose only use of
// the branch tracing enum is to destroy it.
// OPTIONAL-LABEL:  // specialized pullback of tupleRecovery(_:_:_:)
// OPTIONAL:        sil private [signature_optimized_thunk] [heuristic_always_inline] @$s3src13tupleRecoveryySf_SftSf_SfSbtFTJpSSUpSSr00186_47$s3src13tupleRecoveryySf_SftSf_SfSbtFTJrSSUpSSr$s3src033_AD__$s3src13tupleRecoveryySf_Sfte1_e16SbtF_bb4__Pred__A56_0_1_wrt_0_1_spec_bb3_1_feb255e32901dd77cf677bd9520c5b2aOD_cpDAAfJEfJeTf1nnb_nTf4nnd_n : $@convention(thin) (Float, Float) -> (Float, Float) {
// OPTIONAL-NOT:      release_value
// OPTIONAL:        } // end sil function

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

tests.testWithLeakChecking("NormalAndError") {
  let normal = valueWithPullback(at: Float(3), of: { recoveringSquare($0, true) })
  expectEqual(9, normal.value)
  expectEqual(12, normal.pullback(2))

  let error = valueWithPullback(at: Float(-3), of: { recoveringSquare($0, true) })
  expectEqual(-3, error.value)
  expectEqual(2, error.pullback(2))

  let fallback = valueWithPullback(at: Float(-3), of: { recoveringSquare($0, false) })
  expectEqual(9, fallback.value)
  expectEqual(-12, fallback.pullback(2))
}
tests.testWithLeakChecking("OptionalCaptureWithUnusedTape") {
  for (y, flag, first, second): (Float, Bool, Float, Float) in [
    (2, true, 4, 1), (-2, true, 3, 1), (2, false, 4, 6)
  ] {
    let pair = tupleRecovery(3, y, flag)
    expectEqual((first, second), pair)
    let result = valueWithPullback(at: Float(3), y, of: {
      recoverySecond($0, $1, flag)
    })
    expectEqual(second, result.value)
    expectEqual((0, 0), result.pullback(1))
    expectEqual((0, 0), result.pullback(2))
  }
}

// MARK: - Duplicated Optional.none payloads

// CHECK-LABEL:     enum _AD__$s3src11recoverPairySf_SftSf_SfS2btF_bb5__Pred__src_0_1_wrt_0_1_spec_bb3_1_bca753b60370b713fa8dbb3564e8a2ae {
// CHECK-NEXT:        case bb3((predecessor: _AD__$s3src11recoverPairySf_SftSf_SfS2btF_bb3__Pred__src_0_1_wrt_0_1_spec_bb0_0_e6fdeea8b295358a8af69f8bb81066d5, (_: Float)?))
// CHECK-LABEL:     enum _AD__$s3src11recoverPairySf_SftSf_SfS2btF_bb4__Pred__src_0_1_wrt_0_1_spec_bb3_1_e33d23b602d78fab6ed1b91c4738750d {
// CHECK-NEXT:        case bb3((predecessor: _AD__$s3src11recoverPairySf_SftSf_SfS2btF_bb3__Pred__src_0_1_wrt_0_1_spec_bb0_0_e6fdeea8b295358a8af69f8bb81066d5, (_: Float)?))
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

// Delay exposing the callback until jump threading duplicates the error edge.
@_semantics("inline_late")
func squarePullback(_ x: Float) -> (Float) -> Float { { 2 * x * $0 } }

// Jump threading duplicates the second error path. CSE reuses the first
// path's dominating Optional.none, so the duplicate constructors save the
// same SSA values.
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
// CHECK-LABEL:     // reverse-mode derivative of recoverPair(_:_:_:_:)
// CHECK:             // function_ref specialized pullback of recoverPair(_:_:_:_:)
// CHECK-NEXT:        %[[#PULLBACK:]] = function_ref @$s3src11recoverPairySf_SftSf_SfS2btFTJpSSUUpSSr00179_47$s3src11recoverPairySf_SftSf_SfS2btFTJrSSUUpSSr$s3src031_AD__$s3src11recoverPairySf_Sfte1_e17S2btF_bb2__Pred__A50_0_1_wrt_0_1_spec_106201982b1e6f678a66b1fdef5e55e9OD_EDHIGeoxfCeTf1nnb_nTf4nnd_n : $@convention(thin) (Float, Float) -> (Float, Float)
// CHECK-NEXT:        %[[#CLOSURE:]] = thin_to_thick_function %[[#PULLBACK]]
// CHECK-NEXT:        %[[#RESULT:]] = tuple (%[[#]], %[[#]], %[[#CLOSURE]])
// CHECK-NEXT:        return %[[#RESULT]]

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

tests.test("DuplicatedOptionalNone") {
  for a in [false, true] {
    for b in [false, true] {
      for x: Float in [-2, 0, 2] {
        for y: Float in [-2, 0, 2] {
          let result = valueWithPullback(at: x, y, of: { recover($0, $1, a, b) })
          let expectedValue: Float
          let dx: Float
          let dy: Float
          if y >= 0 && !a {
            expectedValue = y * y
            dx = 0
            dy = 2 * y
          } else if x >= 0 && !b {
            expectedValue = x * x
            dx = 2 * x
            dy = 0
          } else if x < 0 {
            expectedValue = y
            dx = 0
            dy = 1
          } else {
            expectedValue = x
            dx = 1
            dy = 0
          }
          precondition(result.value == expectedValue)
          let tag = valueWithPullback(at: x, y, of: { recoveryTag($0, $1, a, b) })
          precondition(tag.value == (y >= 0 && !a ? 1 : x >= 0 && !b ? 2 : 3))
          precondition(tag.pullback(1) == (0, 0))
          for seed: Float in [0, 1, -2, 1] {
            let gradient = result.pullback(seed)
            precondition(gradient.0 == dx * seed && gradient.1 == dy * seed)
          }
        }
      }
    }
  }
}

// MARK: - Trivial tape elimination

// CHECK-LABEL:     // reverse-mode derivative of tupleBranch(_:_:)
// CHECK:             // function_ref specialized pullback of tupleBranch(_:_:)
// CHECK-NEXT:        %[[#PULLBACK:]] = function_ref @$s3src11tupleBranchySf_SftSf_SftFTJpSSpSSr00188_42$s3src11tupleBranchySf_SftSf_SftFTJrSSpSSr$s3src031_AD__$s3src11tupleBranchySf_Sfte1_f13F_bb7__Pred__A68_0_1_wrt_0_1_spec_bb5_1_2_3_bb2_1_2_46494619036b46ed50c95bd5c87a2b00OD_DuDJAfgHfcfTf1nnb_nTf4nnd_n : $@convention(thin) (Float, Float) -> (Float, Float)
// CHECK-NEXT:        %[[#CLOSURE:]] = thin_to_thick_function %[[#PULLBACK]]
// CHECK-NEXT:        %[[#RESULT:]] = tuple (%[[#]], %[[#]], %[[#CLOSURE]])
// CHECK-NEXT:        return %[[#RESULT]]
// CHECK-LABEL:     // specialized pullback of tupleBranch(_:_:)
// CHECK:           sil private [signature_optimized_thunk] [heuristic_always_inline] @$s3src11tupleBranchySf_SftSf_SftFTJpSSpSSr00188_42$s3src11tupleBranchySf_SftSf_SftFTJrSSpSSr$s3src031_AD__$s3src11tupleBranchySf_Sfte1_f13F_bb7__Pred__A68_0_1_wrt_0_1_spec_bb5_1_2_3_bb2_1_2_46494619036b46ed50c95bd5c87a2b00OD_DuDJAfgHfcfTf1nnb_nTf4nnd_n : $@convention(thin) (Float, Float) -> (Float, Float) {
// CHECK-NOT:         release_value
// CHECK:           } // end sil function

// This tuple-result pullback only destroys its branch tracing enum. Replacing
// the stored closures with Float captures makes the enum trivial.
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

tests.test("TrivialBranchTracingEnum") {
  for (x, y, first, second): (Float, Float, Float, Float) in [
    (-1, 2, 12, 1), (-1, 0, 0, 1), (1, 2, 32, 6), (1, 0, 0, 6)
  ] {
    let result = tupleBranch(x, y)
    precondition(result.0 == first)
    precondition(result.1 == second)
    let (value, pullback) = valueWithPullback(at: x, y, of: secondComponent)
    let (dx, dy) = pullback(1)
    precondition(value == second)
    precondition(dx == 0 && dy == 0)
  }
}

runAllTests()

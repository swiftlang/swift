// REQUIRES: executable_test
// RUN: %target-run-simple-swift(-Onone)
// RUN: %target-run-simple-swift(-O)
// RUN: %target-swift-frontend -emit-sil -O -sil-verify-all -module-name src %s -o %t.sil
// RUN: %FileCheck %s < %t.sil

import _Differentiation
import DifferentiationUnittest
import StdlibUnittest

// Keep the iteration count dynamic during optimization.
public var iterationCount = 3

@differentiable(reverse)
@inline(never)
public func square(_ x: Float) -> Float { x * x }

@differentiable(reverse)
public func loop(_ x: Float) -> Float {
  var result = x
  for _ in 0..<iterationCount {
    result = square(result)
  }
  return result
}

public func caller(_ x: Float) -> Float {
  gradient(at: x, of: loop)
}

// Inlining the loop before ADCS would make this VJP ineligible for branch
// tracing enum specialization.
@differentiable(reverse, wrt: x)
public func branchingLoop(_ x: Float, _ flag: Bool) -> Float {
  if flag { return loop(x) }
  return x * x
}

// Preserve branchingLoop's enum for ADCS before inlining its VJP.
@differentiable(reverse, wrt: x)
public func throughBranchingLoop(_ x: Float, _ flag: Bool) -> Float {
  branchingLoop(x, flag)
}

public func branchingCaller(_ x: Float, _ flag: Bool) -> Float {
  gradient(at: x, of: { throughBranchingLoop($0, flag) })
}

@differentiable(reverse)
public func scaleLoop(_ x: Float) -> Float { 2 * loop(x) }

@differentiable(reverse)
public func halvingLoop(_ x: Float) -> Float {
  var value = x
  var remaining = iterationCount
  while remaining > 0 {
    value *= 0.5
    remaining -= 1
  }
  return value
}

@differentiable(reverse)
public func forwardHalvingLoop(_ x: inout Float) { x = halvingLoop(x) }

@differentiable(reverse)
public func throughHalvingLoop(_ x: Float) -> Float {
  var value = x
  forwardHalvingLoop(&value)
  return value
}

@differentiable(reverse, wrt: x)
public func loopOverBranch(_ x: Float, _ flag: Bool) -> Float {
  var result = x
  for _ in 0..<iterationCount {
    result = branchingLoop(result, flag)
  }
  return result
}

// CHECK-LABEL: // reverse-mode derivative of branchingLoop(_:_:)
// CHECK:         // function_ref reverse-mode derivative of loop(_:)
// CHECK-NEXT:    %[[#LOOP_VJP:]] = function_ref
// CHECK-NEXT:    apply %[[#LOOP_VJP]]
// CHECK:         // function_ref specialized pullback of branchingLoop(_:_:)
// CHECK-NEXT:    %[[#BRANCH_FN:]] = function_ref @[[BRANCH_PB:[^ ]+]]
// CHECK-NEXT:    partial_apply [callee_guaranteed] %[[#BRANCH_FN]]
// CHECK:       } // end sil function

// CHECK-LABEL: // reverse-mode derivative of throughBranchingLoop(_:_:)
// CHECK-NOT:     function_ref @$s3src13branchingLoopyS2f_SbtFTJr
// CHECK:         function_ref @[[BRANCH_PB]]
// CHECK-NOT:     function_ref @$s3src13branchingLoopyS2f_SbtFTJr
// CHECK:       } // end sil function

// CHECK-LABEL: // reverse-mode derivative of scaleLoop(_:)
// CHECK-NOT:     function_ref @$s3src4loopyS2fFTJr
// CHECK:         %[[#SCALE_CONTEXT:]] = builtin "autoDiffCreateLinearMapContextWithType"
// CHECK-NOT:     function_ref @$s3src4loopyS2fFTJr
// CHECK:         // function_ref specialized pullback of scaleLoop(_:)
// CHECK-NEXT:    %[[#SCALE_FN:]] = function_ref
// CHECK-NEXT:    partial_apply [callee_guaranteed] %[[#SCALE_FN]](%[[#]], %[[#]], %[[#SCALE_CONTEXT]]) : $@convention(thin) (Float, Float, Float, @owned Builtin.NativeObject) -> Float
// CHECK-NOT:     function_ref @$s3src4loopyS2fFTJr
// CHECK:       } // end sil function

// CHECK-LABEL: // reverse-mode derivative of throughHalvingLoop(_:)
// CHECK-NOT:     function_ref @$s3src18forwardHalvingLoopyySfzFTJr
// CHECK:         %[[#CONTEXT:]] = builtin "autoDiffCreateLinearMapContextWithType"
// CHECK-NOT:     function_ref @$s3src18forwardHalvingLoopyySfzFTJr
// CHECK:         // function_ref specialized pullback of throughHalvingLoop(_:)
// CHECK-NEXT:    %[[#HALVE_FN:]] = function_ref
// CHECK-NEXT:    %[[#HALVE_PB:]] = partial_apply [callee_guaranteed] %[[#HALVE_FN]](%[[#CONTEXT]]) : $@convention(thin) (Float, @owned Builtin.NativeObject) -> Float
// CHECK-NEXT:    %[[#HALVE_RESULT:]] = tuple (%[[#]], %[[#HALVE_PB]])
// CHECK-NEXT:    return %[[#HALVE_RESULT]]
// CHECK-NEXT:  } // end sil function

var tests = TestSuite("LoopVJPInlining")
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

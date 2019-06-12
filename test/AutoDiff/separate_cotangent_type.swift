// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest
#if os(macOS)
import Darwin.C
#else
import Glibc
#endif

var SeparateTangentTypeTests = TestSuite("SeparateTangentType")

struct DifferentiableSubset : Differentiable {
  @differentiable(wrt: self)
  var w: Float
  @differentiable(wrt: self)
  var b: Float
  @noDerivative var flag: Bool

  struct TangentVector : Differentiable, VectorProtocol {
    typealias TangentVector = DifferentiableSubset.TangentVector
    var w: Float
    var b: Float
  }
  func moved(along v: TangentVector) -> DifferentiableSubset {
    return DifferentiableSubset(w: w.moved(along: v.w), b: b.moved(along: v.b), flag: flag)
  }
}

SeparateTangentTypeTests.test("Trivial") {
  let x = DifferentiableSubset(w: 0, b: 1, flag: false)
  let pb = pullback(at: x) { x in x }
  expectEqual(pb(DifferentiableSubset.TangentVector.zero), DifferentiableSubset.TangentVector.zero)
}

SeparateTangentTypeTests.test("Initialization") {
  let x = DifferentiableSubset(w: 0, b: 1, flag: false)
  let pb = pullback(at: x) { x in DifferentiableSubset(w: 1, b: 2, flag: true) }
  expectEqual(pb(DifferentiableSubset.TangentVector.zero), DifferentiableSubset.TangentVector.zero)
}

SeparateTangentTypeTests.test("SomeArithmetics") {
  let x = DifferentiableSubset(w: 0, b: 1, flag: false)
  let pb = pullback(at: x) { x in DifferentiableSubset(w: x.w * x.w, b: x.b * x.b, flag: true) }
  expectEqual(pb(DifferentiableSubset.TangentVector.zero), DifferentiableSubset.TangentVector.zero)
}

runAllTests()

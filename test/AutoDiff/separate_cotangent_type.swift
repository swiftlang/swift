// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest
#if os(macOS)
import Darwin.C
#else
import Glibc
#endif

var SeparateCotangentTypeTests = TestSuite("SeparateCotangentType")

@_fieldwiseDifferentiable
struct DifferentiableSubset : Differentiable {
  @differentiable(wrt: (self))
  var w: Float
  @differentiable(wrt: (self))
  var b: Float
  @noDerivative var flag: Bool

  @_fieldwiseDifferentiable
  struct TangentVector : Differentiable, VectorNumeric {
    typealias TangentVector = DifferentiableSubset.TangentVector
    typealias CotangentVector = DifferentiableSubset.CotangentVector
    var w: Float
    var b: Float
    func tangentVector(from cotan: CotangentVector) -> TangentVector {
      return TangentVector(w: cotan.w, b: cotan.b)
    }
  }
  @_fieldwiseDifferentiable
  struct CotangentVector : Differentiable, VectorNumeric {
    typealias TangentVector = DifferentiableSubset.CotangentVector
    typealias CotangentVector = DifferentiableSubset.TangentVector
    var w: Float
    var b: Float
    func tangentVector(from cotan: CotangentVector) -> TangentVector {
      return TangentVector(w: cotan.w, b: cotan.b)
    }
  }
  func tangentVector(from cotan: CotangentVector) -> TangentVector {
    return TangentVector(w: cotan.w, b: cotan.b)
  }
  func moved(along v: TangentVector) -> DifferentiableSubset {
    return DifferentiableSubset(w: w.moved(along: v.w), b: b.moved(along: v.b), flag: flag)
  }
}

SeparateCotangentTypeTests.test("Trivial") {
  let x = DifferentiableSubset(w: 0, b: 1, flag: false)
  let pb = pullback(at: x) { x in x }
  expectEqual(pb(DifferentiableSubset.CotangentVector.zero), DifferentiableSubset.CotangentVector.zero)
}

SeparateCotangentTypeTests.test("Initialization") {
  let x = DifferentiableSubset(w: 0, b: 1, flag: false)
  let pb = pullback(at: x) { x in DifferentiableSubset(w: 1, b: 2, flag: true) }
  expectEqual(pb(DifferentiableSubset.CotangentVector.zero), DifferentiableSubset.CotangentVector.zero)
}

// FIXME(SR-9602): If `CotangentVector` is not marked
// `@_fieldwiseProductSpace`, call the VJP of the memberwise initializer.
// SeparateCotangentTypeTests.test("SomeArithmetics") {
//   let x = DifferentiableSubset(w: 0, b: 1, flag: false)
//   let pb = pullback(at: x) { x in DifferentiableSubset(w: x.w * x.w, b: x.b * x.b, flag: true) }
//   expectEqual(pb(DifferentiableSubset.CotangentVector.zero), DifferentiableSubset.CotangentVector.zero)
// }

runAllTests()

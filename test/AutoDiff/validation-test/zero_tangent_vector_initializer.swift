// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import _Differentiation
import StdlibUnittest

var ZeroTangentVectorTests = TestSuite("zeroTangentVectorInitializer")

struct Generic<T: Differentiable, U: Differentiable>: Differentiable {
  var x: T
  var y: U
}

struct Nested<T: Differentiable, U: Differentiable>: Differentiable {
  var generic: Generic<T, U>
}

ZeroTangentVectorTests.test("Derivation") {
  typealias G = Generic<[Float], [[Float]]>

  let generic = G(x: [1, 2, 3], y: [[4, 5, 6], [], [2]])
  let genericZero = G.TangentVector(x: [0, 0, 0], y: [[0, 0, 0], [], [0]])
  expectEqual(generic.zeroTangentVector, genericZero)

  let nested = Nested(generic: generic)
  let nestedZero = Nested.TangentVector(generic: genericZero)
  expectEqual(nested.zeroTangentVector, nestedZero)
}

// Test differentiation correctness involving projection operations and
// per-instance zeros.
ZeroTangentVectorTests.test("DifferentiationCorrectness") {
  struct Struct: Differentiable {
    var x, y: [Float]
  }
  func concatenated(_ lhs: Struct, _ rhs: Struct) -> Struct {
    return Struct(x: lhs.x + rhs.x, y: lhs.y + rhs.y)
  }
  func test(_ s: Struct) -> [Float] {
    let result = concatenated(s, s).withDerivative { dresult in
      // FIXME(TF-1008): Fix incorrect derivative values for
      // "projection operation" operands when differentiation transform uses
      // `Differentiable.zeroTangentVectorInitializer`.
      //   Actual: TangentVector(x: [1.0, 1.0, 1.0], y: [])
      // Expected: TangentVector(x: [1.0, 1.0, 1.0], y: [1.0, 1.0, 1.0])
      expectEqual(dresult, Struct.TangentVector(x: [1, 1, 1], y: [1, 1, 1]))
    }
    return result.x
  }
  let s = Struct(x: [1, 2, 3], y: [1, 2, 3])
  let pb = pullback(at: s, in: test)
  // FIXME(TF-1008): Remove `expectCrash` when differentiation transform uses
  // `Differentiable.zeroTangentVectorInitializer`.
  expectCrash {
    _ = pb([1, 1, 1])
  }
}

runAllTests()

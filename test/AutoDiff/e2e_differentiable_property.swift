// RUN: %target-run-simple-swift
// REQUIRES: executable_test

// An end-to-end test that we can differentiate property accesses, with custom
// VJPs for the properties specified in various ways.

import StdlibUnittest

var E2EDifferentiablePropertyTests = TestSuite("E2EDifferentiableProperty")

struct TangentSpace : VectorNumeric {
  let dx, dy: Float
}

extension TangentSpace : Differentiable {
  typealias TangentVector = TangentSpace
  typealias CotangentVector = TangentSpace
}

struct Space {
  /// `x` is a computed property with a custom vjp.
  var x: Float {
    @differentiable(vjp: vjpX)
    get {
      return storedX
    }
  }

  func vjpX() -> (Float, (Float) -> TangentSpace) {
    return (x, { v in TangentSpace(dx: v, dy: 0) } )
  }

  private let storedX: Float

  /// `y` is a stored property with a custom vjp for its getter.
  @differentiable(vjp: vjpY)
  let y: Float

  func vjpY() -> (Float, (Float) -> TangentSpace) {
    return (y, { v in TangentSpace(dx: 0, dy: v) })
  }

  init(x: Float, y: Float) {
    self.storedX = x
    self.y = y
  }
}

extension Space : Differentiable {
  typealias TangentVector = TangentSpace
  typealias CotangentVector = TangentSpace
  func moved(along: TangentSpace) -> Space {
    return Space(x: x + along.dx, y: y + along.dy)
  }
  func tangentVector(from cotangent: CotangentVector) -> TangentVector {
    return cotangent
  }
}

E2EDifferentiablePropertyTests.test("computed property") {
  let actualGrad = gradient(at: Space(x: 0, y: 0)) { (point: Space) -> Float in
    return 2 * point.x
  }
  let expectedGrad = TangentSpace(dx: 2, dy: 0)
  expectEqual(expectedGrad, actualGrad)
}

E2EDifferentiablePropertyTests.test("stored property") {
  let actualGrad = gradient(at: Space(x: 0, y: 0)) { (point: Space) -> Float in
    return 3 * point.y
  }
  let expectedGrad = TangentSpace(dx: 0, dy: 3)
  expectEqual(expectedGrad, actualGrad)
}

struct ProductSpaceSelfTangent : VectorNumeric {
  let x, y: Float
}

extension ProductSpaceSelfTangent : Differentiable {
  @_fieldwiseProductSpace
  typealias TangentVector = ProductSpaceSelfTangent
  @_fieldwiseProductSpace
  typealias CotangentVector = ProductSpaceSelfTangent
}

E2EDifferentiablePropertyTests.test("fieldwise product space, self tangent") {
  let actualGrad = gradient(at: ProductSpaceSelfTangent(x: 0, y: 0)) { (point: ProductSpaceSelfTangent) -> Float in
    return 5 * point.y
  }
  let expectedGrad = ProductSpaceSelfTangent(x: 0, y: 5)
  expectEqual(expectedGrad, actualGrad)
}

struct ProductSpaceOtherTangentTangentSpace : VectorNumeric {
  let x, y: Float
}

extension ProductSpaceOtherTangentTangentSpace : Differentiable {
  typealias TangentVector = ProductSpaceOtherTangentTangentSpace
  typealias CotangentVector = ProductSpaceOtherTangentTangentSpace
}

struct ProductSpaceOtherTangent {
  let x, y: Float
}

extension ProductSpaceOtherTangent : Differentiable {
  @_fieldwiseProductSpace
  typealias TangentVector = ProductSpaceOtherTangentTangentSpace
  @_fieldwiseProductSpace
  typealias CotangentVector = ProductSpaceOtherTangentTangentSpace
  func moved(along: ProductSpaceOtherTangentTangentSpace) -> ProductSpaceOtherTangent {
    return ProductSpaceOtherTangent(x: x + along.x, y: y + along.y)
  }
  func tangentVector(from cotangent: CotangentVector) -> TangentVector {
    return cotangent
  }
}

E2EDifferentiablePropertyTests.test("fieldwise product space, other tangent") {
  let actualGrad = gradient(at: ProductSpaceOtherTangent(x: 0, y: 0)) { (point: ProductSpaceOtherTangent) -> Float in
    return 7 * point.y
  }
  let expectedGrad = ProductSpaceOtherTangentTangentSpace(x: 0, y: 7)
  expectEqual(expectedGrad, actualGrad)
}

runAllTests()

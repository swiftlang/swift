// RUN: %target-run-simple-swift
// REQUIRES: executable_test

// An end-to-end test that we can differentiate property accesses, with custom
// VJPs for the properties specified in various ways.

import StdlibUnittest
import DifferentiationUnittest

var E2EDifferentiablePropertyTests = TestSuite("E2EDifferentiableProperty")

struct TangentSpace : AdditiveArithmetic {
  let x, y: Float
}

extension TangentSpace : Differentiable {
  typealias TangentVector = TangentSpace
}

struct Space {
  /// `x` is a computed property with a custom vjp.
  var x: Float {
    @differentiable(reverse)
    get { storedX }
    set { storedX = newValue }
  }

  @derivative(of: x)
  func vjpX() -> (value: Float, pullback: (Float) -> TangentSpace) {
    return (x, { v in TangentSpace(x: v, y: 0) } )
  }

  private var storedX: Float

  @differentiable(reverse)
  var y: Float

  init(x: Float, y: Float) {
    self.storedX = x
    self.y = y
  }
}

extension Space : Differentiable {
  typealias TangentVector = TangentSpace
  mutating func move(by offset: TangentSpace) {
    x.move(by: offset.x)
    y.move(by: offset.y)
  }
}

E2EDifferentiablePropertyTests.test("computed property") {
  let actualGrad = gradient(at: Space(x: 0, y: 0)) { (point: Space) -> Float in
    return 2 * point.x
  }
  let expectedGrad = TangentSpace(x: 2, y: 0)
  expectEqual(expectedGrad, actualGrad)
}

E2EDifferentiablePropertyTests.test("stored property") {
  let actualGrad = gradient(at: Space(x: 0, y: 0)) { (point: Space) -> Float in
    return 3 * point.y
  }
  let expectedGrad = TangentSpace(x: 0, y: 3)
  expectEqual(expectedGrad, actualGrad)
}

struct GenericMemberWrapper<T : Differentiable> : Differentiable {
  // Stored property.
  @differentiable(reverse)
  var x: T

  func vjpX() -> (T, (T.TangentVector) -> GenericMemberWrapper.TangentVector) {
    return (x, { TangentVector(x: $0) })
  }
}

E2EDifferentiablePropertyTests.test("generic stored property") {
  let actualGrad = gradient(at: GenericMemberWrapper<Float>(x: 1)) { point in
    return 2 * point.x
  }
  let expectedGrad = GenericMemberWrapper<Float>.TangentVector(x: 2)
  expectEqual(expectedGrad, actualGrad)
}

struct ProductSpaceSelfTangent : AdditiveArithmetic {
  let x, y: Float
}

extension ProductSpaceSelfTangent : Differentiable {
  typealias TangentVector = ProductSpaceSelfTangent
}

E2EDifferentiablePropertyTests.test("fieldwise product space, self tangent") {
  let actualGrad = gradient(at: ProductSpaceSelfTangent(x: 0, y: 0)) { (point: ProductSpaceSelfTangent) -> Float in
    return 5 * point.y
  }
  let expectedGrad = ProductSpaceSelfTangent(x: 0, y: 5)
  expectEqual(expectedGrad, actualGrad)
}

struct ProductSpaceOtherTangentTangentSpace : AdditiveArithmetic {
  let x, y: Float
}

extension ProductSpaceOtherTangentTangentSpace : Differentiable {
  typealias TangentVector = ProductSpaceOtherTangentTangentSpace
}

struct ProductSpaceOtherTangent {
  var x, y: Float
}

extension ProductSpaceOtherTangent : Differentiable {
  typealias TangentVector = ProductSpaceOtherTangentTangentSpace
  mutating func move(by offset: ProductSpaceOtherTangentTangentSpace) {
    x.move(by: offset.x)
    y.move(by: offset.y)
  }
}

E2EDifferentiablePropertyTests.test("fieldwise product space, other tangent") {
  let actualGrad = gradient(
    at: ProductSpaceOtherTangent(x: 0, y: 0)
  ) { (point: ProductSpaceOtherTangent) -> Float in
    return 7 * point.y
  }
  let expectedGrad = ProductSpaceOtherTangentTangentSpace(x: 0, y: 7)
  expectEqual(expectedGrad, actualGrad)
}

E2EDifferentiablePropertyTests.test("computed property") {
  struct TF_544 : Differentiable {
    var value: Float
    @differentiable(reverse)
    var computed: Float {
      get { value }
      set { value = newValue }
    }
  }
  let actualGrad = gradient(at: TF_544(value: 2.4)) { x in
    return x.computed * x.computed
  }
  let expectedGrad = TF_544.TangentVector(value: 4.8)
  expectEqual(expectedGrad, actualGrad)
}

/* Temporary disabled until https://github.com/swiftlang/swift/issues/84840 is fixed
   We cannot use `Tracked<T>` :(
struct TangentSpace : AdditiveArithmetic {
  let x, y: Tracked<Float>
}

extension TangentSpace : Differentiable {
  typealias TangentVector = TangentSpace
}

struct Space {
  /// `x` is a computed property with a custom vjp.
  var x: Tracked<Float> {
    @differentiable(reverse)
    get { storedX }
    set { storedX = newValue }
  }

  @derivative(of: x)
  func vjpX() -> (value: Tracked<Float>, pullback: (Tracked<Float>) -> TangentSpace) {
    return (x, { v in TangentSpace(x: v, y: 0) } )
  }

  private var storedX: Tracked<Float>

  @differentiable(reverse)
  var y: Tracked<Float>

  init(x: Tracked<Float>, y: Tracked<Float>) {
    self.storedX = x
    self.y = y
  }
}

extension Space : Differentiable {
  typealias TangentVector = TangentSpace
  mutating func move(by offset: TangentSpace) {
    x.move(by: offset.x)
    y.move(by: offset.y)
  }
}

E2EDifferentiablePropertyTests.testWithLeakChecking("computed property") {
  let actualGrad = gradient(at: Space(x: 0, y: 0)) { (point: Space) -> Tracked<Float> in
    return 2 * point.x
  }
  let expectedGrad = TangentSpace(x: 2, y: 0)
  expectEqual(expectedGrad, actualGrad)
}

E2EDifferentiablePropertyTests.testWithLeakChecking("stored property") {
  let actualGrad = gradient(at: Space(x: 0, y: 0)) { (point: Space) -> Tracked<Float> in
    return 3 * point.y
  }
  let expectedGrad = TangentSpace(x: 0, y: 3)
  expectEqual(expectedGrad, actualGrad)
}

struct GenericMemberWrapper<T : Differentiable> : Differentiable {
  // Stored property.
  @differentiable(reverse)
  var x: T

  func vjpX() -> (T, (T.TangentVector) -> GenericMemberWrapper.TangentVector) {
    return (x, { TangentVector(x: $0) })
  }
}

E2EDifferentiablePropertyTests.testWithLeakChecking("generic stored property") {
  let actualGrad = gradient(at: GenericMemberWrapper<Tracked<Float>>(x: 1)) { point in
    return 2 * point.x
  }
  let expectedGrad = GenericMemberWrapper<Tracked<Float>>.TangentVector(x: 2)
  expectEqual(expectedGrad, actualGrad)
}

struct ProductSpaceSelfTangent : AdditiveArithmetic {
  let x, y: Tracked<Float>
}

extension ProductSpaceSelfTangent : Differentiable {
  typealias TangentVector = ProductSpaceSelfTangent
}

E2EDifferentiablePropertyTests.testWithLeakChecking("fieldwise product space, self tangent") {
  let actualGrad = gradient(at: ProductSpaceSelfTangent(x: 0, y: 0)) { (point: ProductSpaceSelfTangent) -> Tracked<Float> in
    return 5 * point.y
  }
  let expectedGrad = ProductSpaceSelfTangent(x: 0, y: 5)
  expectEqual(expectedGrad, actualGrad)
}

struct ProductSpaceOtherTangentTangentSpace : AdditiveArithmetic {
  let x, y: Tracked<Float>
}

extension ProductSpaceOtherTangentTangentSpace : Differentiable {
  typealias TangentVector = ProductSpaceOtherTangentTangentSpace
}

struct ProductSpaceOtherTangent {
  var x, y: Tracked<Float>
}

extension ProductSpaceOtherTangent : Differentiable {
  typealias TangentVector = ProductSpaceOtherTangentTangentSpace
  mutating func move(by offset: ProductSpaceOtherTangentTangentSpace) {
    x.move(by: offset.x)
    y.move(by: offset.y)
  }
}

E2EDifferentiablePropertyTests.testWithLeakChecking("fieldwise product space, other tangent") {
  let actualGrad = gradient(
    at: ProductSpaceOtherTangent(x: 0, y: 0)
  ) { (point: ProductSpaceOtherTangent) -> Tracked<Float> in
    return 7 * point.y
  }
  let expectedGrad = ProductSpaceOtherTangentTangentSpace(x: 0, y: 7)
  expectEqual(expectedGrad, actualGrad)
}

E2EDifferentiablePropertyTests.testWithLeakChecking("computed property") {
  struct TF_544 : Differentiable {
    var value: Tracked<Float>
    @differentiable(reverse)
    var computed: Tracked<Float> {
      get { value }
      set { value = newValue }
    }
  }
  let actualGrad = gradient(at: TF_544(value: 2.4)) { x in
    return x.computed * x.computed
  }
  let expectedGrad = TF_544.TangentVector(value: 4.8)
  expectEqual(expectedGrad, actualGrad)
}
*/

runAllTests()

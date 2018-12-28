// RUN: %target-run-simple-swift
// REQUIRES: executable_test

// An end-to-end test that we can differentiate property accesses, with custom
// VJPs for the properties specified in various ways.

import StdlibUnittest

var E2EDifferentiablePropertyTests = TestSuite("E2EDifferentiableProperty")

struct TangentSpace {
  let dx, dy: Float
}

extension TangentSpace : Differentiable, VectorNumeric {
  typealias TangentVector = TangentSpace
  typealias CotangentVector = TangentSpace
  typealias Scalar = Float
  static var zero: TangentSpace {
    return TangentSpace(dx: 0, dy: 0)
  }
  static func + (lhs: TangentSpace, rhs: TangentSpace) -> TangentSpace {
    return TangentSpace(dx: lhs.dx + rhs.dx, dy: lhs.dy + rhs.dy)
  }
  static func - (lhs: TangentSpace, rhs: TangentSpace) -> TangentSpace {
    return TangentSpace(dx: lhs.dx - rhs.dx, dy: lhs.dy - rhs.dy)
  }
  static func * (lhs: Float, rhs: TangentSpace) -> TangentSpace {
    return TangentSpace(dx: lhs * rhs.dx, dy: lhs * rhs.dy)
  }
}

struct Space {
  /// `x` is a computed property with a custom vjp.
  var x: Float {
    @differentiable(wrt: (self), vjp: vjpX)
    get {
      return storedX
    }
  }

  func vjpX() -> (Float, (Float) -> TangentSpace) {
    return (x, { v in TangentSpace(dx: v, dy: 0) } )
  }

  private let storedX: Float

  /// `y` is a stored property with a custom vjp for its getter.
  @differentiable(wrt: (self), vjp: vjpY)
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
}

E2EDifferentiablePropertyTests.test("computed property") {
  let actualGrad = gradient(at: Space(x: 0, y: 0)) { (point: Space) -> Float in
    return 2 * point.x
  }
  let expectedGrad = TangentSpace(dx: 2, dy: 0)
  expectEqual(expectedGrad, actualGrad)
}

// FIXME: The AD pass cannot differentiate this because it sees
// `struct_extract`s instead of calls to getters. This problem should fix
// itself once we move the AD pass before mandatory inlining, and we should be
// able to enable this test.
// E2EDifferentiablePropertyTests.test("stored property") {
//   let actualGrad = gradient(at: Space(x: 0, y: 0)) { (point: Space) -> Float in
//     return 3 * point.y
//   }
//   let expectedGrad = TangentSpace(dx: 0, dy: 3)
//   expectEqual(expectedGrad, actualGrad)
// }

runAllTests()

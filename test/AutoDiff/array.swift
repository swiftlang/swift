// RUN: %target-run-simple-swift

import StdlibUnittest

var ArrayAutodiffTests = TestSuite("ArrayAutodiff")

ArrayAutodiffTests.test("ArrayIdentity") {
  func arrayIdentity(_ x: [Float]) -> [Float] {
    return x
  }
  let backprop = pullback(at: [1, 2, 3, 4], in: arrayIdentity)
  expectEqual([1.0, 1.0, 1.0, 1.0], backprop(.zero).elements)
}

ArrayAutodiffTests.test("ArraySubscript") {
  func sumFirstThree(_ array: [Float]) -> Float {
    return array[0] + array[1] + array[2]
  }

  expectEqual(
    [1.0, 1.0, 1.0, 0.0, 0.0, 0.0],
    gradient(at: [0.5, 0.2, 0.3, 0.7, 0.6, 1.0], in: sumFirstThree).elements)
}

struct Parameter : Equatable {
  @differentiable(wrt: (self), vjp: vjpX)
  let x: Float

  func vjpX() -> (Float, (Float) -> Parameter) {
    return (x, { dx in Parameter(x: dx) } )
  }
}

extension Parameter {
  func squared() -> Float {
    return x * x
  }

  static func * (_ a: Parameter, _ b: Parameter) -> Float {
    return a.x * b.x
  }
}

extension Parameter : Differentiable, VectorNumeric {
  typealias TangentVector = Parameter
  typealias CotangentVector = Parameter
  typealias Scalar = Float
  typealias Shape = ()

  init(repeating repeatedValue: Float, shape: ()) {
    self.init(x: repeatedValue)
  }

  static func + (lhs: Parameter, rhs: Parameter) -> Parameter {
    return Parameter(x: lhs.x + rhs.x)
  }

  static func - (lhs: Parameter, rhs: Parameter) -> Parameter {
    return Parameter(x: lhs.x - rhs.x)
  }

  static func * (lhs: Scalar, rhs: Parameter) -> Parameter {
    return Parameter(x: lhs * rhs.x)
  }

  static var zero: Parameter { return Parameter(x: 0) }
}

ArrayAutodiffTests.test("ArraySimpleMethod") {
  func f(_ p: [Parameter]) -> Float {
    return 100 * p[0].squared()
  }
  expectEqual([Parameter(x: 4 * 100)], 
              gradient(at: [Parameter(x: 2)], in: f).elements)
  expectEqual([Parameter(x: 40 * 100)], 
              gradient(at: [Parameter(x: 20)], in: f).elements)
}

ArrayAutodiffTests.test("ArrayPairMethod") {
  let g = { (a: [Parameter]) in a[0] * a[1] }
  expectEqual([Parameter(x: 100), Parameter(x: 200)],
              gradient(
                at: [Parameter(x: 200), Parameter(x: 100)], 
                in: g
              ).elements)
  expectEqual([Parameter(x: 200), Parameter(x: 100)],
              gradient(
                at: [Parameter(x: 100), Parameter(x: 200)], 
                in: g
              ).elements)
}

runAllTests()

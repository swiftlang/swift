// RUN: %target-run-simple-swift

import StdlibUnittest
import DifferentiationUnittest

var ArrayAutoDiffTests = TestSuite("ArrayAutoDiff")

typealias FloatArrayGrad = Array<Float>.TangentVector

ArrayAutoDiffTests.test("ArrayIdentity") {
  func arrayIdentity(_ x: [Float]) -> [Float] {
    return x
  }

  let backprop = pullback(at: [5, 6, 7, 8], in: arrayIdentity)
  expectEqual(
    FloatArrayGrad([1, 2, 3, 4]),
    backprop(FloatArrayGrad([1, 2, 3, 4])))
}

ArrayAutoDiffTests.test("ArraySubscript") {
  func sumFirstThree(_ array: [Float]) -> Float {
    return array[0] + array[1] + array[2]
  }

  expectEqual(
    FloatArrayGrad([1, 1, 1, 0, 0, 0]),
    gradient(at: [2, 3, 4, 5, 6, 7], in: sumFirstThree))
}

ArrayAutoDiffTests.test("ArrayLiteral") {
  func twoElementLiteral(_ x: Tracked<Float>, _ y: Tracked<Float>) -> [Tracked<Float>] {
    return [x, y]
  }

  let (gradX, gradY) = pullback(at: Tracked<Float>(1), Tracked<Float>(1), in: twoElementLiteral)(
    Array<Tracked<Float>>.TangentVector([Tracked<Float>(1), Tracked<Float>(2)]))

  expectEqual(Tracked<Float>(1), gradX)
  expectEqual(Tracked<Float>(2), gradY)
}

ArrayAutoDiffTests.test("ArrayLiteralIndirect") {
  func twoElementLiteralIndirect<T: Differentiable & AdditiveArithmetic>(_ x: T, _ y: T) -> [T] {
    return [x, y]
  }

  func twoElementLiteralIndirectWrapper(_ x: Float, _ y: Float) -> [Float] {
    return twoElementLiteralIndirect(x, y)
  }

  let (gradX, gradY) = pullback(at: Float(1), Float(1), in: twoElementLiteralIndirectWrapper)(
    Array<Float>.TangentVector([Float(1), Float(2)]))

  expectEqual(Float(1), gradX)
  expectEqual(Float(2), gradY)
}

ArrayAutoDiffTests.test("ArrayConcat") {
  struct TwoArrays : Differentiable {
    var a: [Float]
    var b: [Float]
  }

  func sumFirstThreeConcatted(_ arrs: TwoArrays) -> Float {
    let c = arrs.a + arrs.b
    return c[0] + c[1] + c[2]
  }

  expectEqual(
    TwoArrays.TangentVector(
      a: FloatArrayGrad([1, 1]),
      b: FloatArrayGrad([1, 0])),
    gradient(
      at: TwoArrays(a: [0, 0], b: [0, 0]),
      in: sumFirstThreeConcatted))
  expectEqual(
    TwoArrays.TangentVector(
      a: FloatArrayGrad([1, 1, 1, 0]),
      b: FloatArrayGrad([0, 0])),
    gradient(
      at: TwoArrays(a: [0, 0, 0, 0], b: [0, 0]),
      in: sumFirstThreeConcatted))
  expectEqual(
    TwoArrays.TangentVector(
      a: FloatArrayGrad([]),
      b: FloatArrayGrad([1, 1, 1, 0])),
    gradient(
      at: TwoArrays(a: [], b: [0, 0, 0, 0]),
      in: sumFirstThreeConcatted))
}

// TODO(TF-768): Re-enable when `init(_:)` is differentiable.
// ArrayAutoDiffTests.test("Array.TangentVector.init") {
//   @differentiable
//   func constructView(_ x: [Float]) -> Array<Float>.TangentVector {
//     return Array<Float>.TangentVector(x)
//   }
//   let backprop = pullback(at: [5, 6, 7, 8], in: constructView)
//   expectEqual(
//     FloatArrayGrad([1, 2, 3, 4]),
//     backprop(FloatArrayGrad([1, 2, 3, 4])))
// }

// TODO(TF-768): Re-enable when `elements` is differentiable.
// ArrayAutoDiffTests.test("Array.TangentVector.elements") {
//   @differentiable
//   func accessElements(_ x: Array<Float>.TangentVector) -> [Float] {
//     return x.elements
//   }
//   let backprop = pullback(
//     at: Array<Float>.TangentVector([5, 6, 7, 8]),
//     in: accessElements)
//   expectEqual(
//     FloatArrayGrad([1, 2, 3, 4]),
//     backprop(FloatArrayGrad([1, 2, 3, 4])))
// }

ArrayAutoDiffTests.test("Array.TangentVector : KeyPathIterable") {
  struct Container : KeyPathIterable {
    let a: Array<Float>.TangentVector
  }
  let container = Container(a: Array<Float>.TangentVector([1, 2, 3]))
  expectEqual(
    [1, 2, 3],
    container.recursivelyAllKeyPaths(to: Float.self).map {
      container[keyPath: $0]
    })
}

runAllTests()

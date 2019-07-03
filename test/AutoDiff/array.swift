// RUN: %target-run-simple-swift

import StdlibUnittest

var ArrayAutodiffTests = TestSuite("ArrayAutodiff")

typealias FloatArrayGrad = Array<Float>.TangentVector

ArrayAutodiffTests.test("ArrayIdentity") {
  func arrayIdentity(_ x: [Float]) -> [Float] {
    return x
  }

  let backprop = pullback(at: [5, 6, 7, 8], in: arrayIdentity)
  expectEqual(
    FloatArrayGrad([1, 2, 3, 4]),
    backprop(FloatArrayGrad([1, 2, 3, 4])))
}

ArrayAutodiffTests.test("ArraySubscript") {
  func sumFirstThree(_ array: [Float]) -> Float {
    return array[0] + array[1] + array[2]
  }

  expectEqual(
    FloatArrayGrad([1, 1, 1, 0, 0, 0]),
    gradient(at: [2, 3, 4, 5, 6, 7], in: sumFirstThree))
}

ArrayAutodiffTests.test("ArrayLiteral") {
  func twoElementLiteral(_ x: Float, _ y: Float) -> [Float] {
    return [x, y]
  }

  let (gradX, gradY) = pullback(at: Float(1), Float(1), in: twoElementLiteral)(
    Array<Float>.DifferentiableView([Float(1), Float(2)]))

  expectEqual(gradX, Float(1))
  expectEqual(gradY, Float(2))
}

ArrayAutodiffTests.test("ArrayLiteralIndirect") {
  func twoElementLiteralIndirect<T: Differentiable & AdditiveArithmetic>(_ x: T, _ y: T) -> [T] {
    return [x, y]
  }

  func twoElementLiteralIndirectWrapper(_ x: Float, _ y: Float) -> [Float] {
    return twoElementLiteralIndirect(x, y)
  }

  let (gradX, gradY) = pullback(at: Float(1), Float(1), in: twoElementLiteralIndirectWrapper)(
    Array<Float>.DifferentiableView([Float(1), Float(2)]))

  expectEqual(gradX, Float(1))
  expectEqual(gradY, Float(2))
}

ArrayAutodiffTests.test("ArrayConcat") {
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

ArrayAutodiffTests.test("Array.DifferentiableView.init") {
  @differentiable
  func constructView(_ x: [Float]) -> Array<Float>.DifferentiableView {
    return Array<Float>.DifferentiableView(x)
  }

  let backprop = pullback(at: [5, 6, 7, 8], in: constructView)
  expectEqual(
    FloatArrayGrad([1, 2, 3, 4]),
    backprop(FloatArrayGrad([1, 2, 3, 4])))
}

ArrayAutodiffTests.test("Array.DifferentiableView.base") {
  @differentiable
  func accessBase(_ x: Array<Float>.DifferentiableView) -> [Float] {
    return x.base
  }

  let backprop = pullback(
    at: Array<Float>.DifferentiableView([5, 6, 7, 8]),
    in: accessBase)
  expectEqual(
    FloatArrayGrad([1, 2, 3, 4]),
    backprop(FloatArrayGrad([1, 2, 3, 4])))
}

ArrayAutodiffTests.test("Array.DifferentiableView : KeyPathIterable") {
  struct Container : KeyPathIterable {
    let a: Array<Float>.DifferentiableView
  }
  let container = Container(a: Array<Float>.DifferentiableView([1, 2, 3]))
  expectEqual(
    [1, 2, 3],
    container.recursivelyAllKeyPaths(to: Float.self).map {
      container[keyPath: $0]
    })
}

runAllTests()

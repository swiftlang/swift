// RUN: %target-run-simple-swift

import StdlibUnittest
import DifferentiationUnittest

var ArrayAutoDiffTests = TestSuite("ArrayAutoDiff")

typealias FloatArrayTan = Array<Float>.TangentVector
typealias TrackedFloatArrayTan = Array<Tracked<Float>>.TangentVector

ArrayAutoDiffTests.test("ArrayIdentity") {
  func arrayIdentity(_ x: [Float]) -> [Float] {
    return x
  }

  let backprop = pullback(at: [5, 6, 7, 8], in: arrayIdentity)
  expectEqual(
    FloatArrayTan([1, 2, 3, 4]),
    backprop(FloatArrayTan([1, 2, 3, 4])))
}

ArrayAutoDiffTests.test("ArraySubscript") {
  func sumFirstThree(_ array: [Float]) -> Float {
    return array[0] + array[1] + array[2]
  }

  expectEqual(
    FloatArrayTan([1, 1, 1, 0, 0, 0]),
    gradient(at: [2, 3, 4, 5, 6, 7], in: sumFirstThree))
}

ArrayAutoDiffTests.testWithLeakChecking("ArrayLiteral") {
  do {
    func twoElementLiteral(_ x: Tracked<Float>, _ y: Tracked<Float>) -> [Tracked<Float>] {
      return [x, y]
    }
    let pb = pullback(at: 1, 1, in: twoElementLiteral)
    let (gradX, gradY) = pb(TrackedFloatArrayTan([Tracked<Float>(1), Tracked<Float>(2)]))
    expectEqual(1, gradX)
    expectEqual(2, gradY)
  }

  do {
    // TF-952: Test array literal initialized from an address (e.g. `var`).
    func twoElementLiteralAddress(_ x: Tracked<Float>, _ y: Tracked<Float>) -> [Tracked<Float>] {
      var result = x
      result = result * y
      return [result, result]
    }
    let pb = pullback(at: 3, 4, in: twoElementLiteralAddress)
    let (gradX, gradY) = pb(TrackedFloatArrayTan([1, 1]))
    expectEqual(8, gradX)
    expectEqual(6, gradY)
  }

  do {
    // TF-952: Test array literal initialized with function call results.
    func twoElementLiteralFunctionResult(_ x: Tracked<Float>, _ y: Tracked<Float>) -> [Tracked<Float>] {
      return [x * y, x * y]
    }
    let pb = pullback(at: 3, 4, in: twoElementLiteralFunctionResult)
    let (gradX, gradY) = pb(TrackedFloatArrayTan([1, 1]))
    // FIXME(TF-952): Fix incorrect zero derivatives.
    // expectEqual(8, gradX)
    // expectEqual(6, gradY)
    expectEqual(0, gradX)
    expectEqual(0, gradY)
  }
}

ArrayAutoDiffTests.test("ArrayLiteralIndirect") {
  func twoElementLiteralIndirect<T: Differentiable & AdditiveArithmetic>(_ x: T, _ y: T) -> [T] {
    return [x, y]
  }

  func twoElementLiteralIndirectWrapper(_ x: Float, _ y: Float) -> [Float] {
    return twoElementLiteralIndirect(x, y)
  }

  let (gradX, gradY) = pullback(at: Float(1), Float(1), in: twoElementLiteralIndirectWrapper)(
    Array<Float>.DifferentiableView([Float(1), Float(2)]))

  expectEqual(Float(1), gradX)
  expectEqual(Float(2), gradY)
}

ArrayAutoDiffTests.testWithLeakChecking("ArrayLiteralControlFlow") {
  do {
    // TF-922: Test array literal and control flow.
    func controlFlow(
        _ x: Tracked<Float>, _ y: Tracked<Float>, _ bool: Bool = true
    ) -> [Tracked<Float>] {
      var result = [x * y, x * y]
      let result2 = bool ? result : result
      var result3 = bool ? (bool ? result2 : result) : result2
      return result3
    }
    let pb = pullback(at: 3, 4, in: { controlFlow($0, $1) })
    let (gradX, gradY) = pb(TrackedFloatArrayTan([1, 1]))
    // FIXME(TF-952): Fix incorrect zero derivatives.
    // expectEqual(8, gradX)
    // expectEqual(6, gradY)
    expectEqual(0, gradX)
    expectEqual(0, gradY)
  }

  do {
    // TF-922: Test array literal and control flow.
    func controlFlowAddress(
        _ x: Tracked<Float>, _ y: Tracked<Float>, _ bool: Bool = true
    ) -> [Tracked<Float>] {
      var product = x * y // initial value is an address
      var result = [product, product]
      let result2 = bool ? result : result
      var result3 = bool ? (bool ? result2 : result) : result2
      return result3
    }
    let pb = pullback(at: 3, 4, in: { controlFlowAddress($0, $1) })
    let (gradX, gradY) = pb(TrackedFloatArrayTan([1, 1]))
    expectEqual(8, gradX)
    expectEqual(6, gradY)
  }

  do {
    // TF-922: Test array literal and control flow.
    func controlFlowGeneric<T>(_ x: T, _ y: T, _ bool: Bool = true) -> [T] {
      var result = [x, y] // initial values are addresses
      let result2 = bool ? result : result
      var result3 = bool ? (bool ? result2 : result) : result2
      return result3
    }
    let pb = pullback(at: Tracked<Float>(3), 4, in: { controlFlowGeneric($0, $1) })
    let (gradX, gradY) = pb(TrackedFloatArrayTan([1, 1]))
    expectEqual(1, gradX)
    expectEqual(1, gradY)
  }
}

ArrayAutoDiffTests.test("ExpressibleByArrayLiteralIndirect") {
  struct Indirect<T: Differentiable>: Differentiable & ExpressibleByArrayLiteral {
    var x: T

    typealias ArrayLiteralElement = T
    init(arrayLiteral: T...) {
      assert(arrayLiteral.count > 1)
      self.x = arrayLiteral[0]
    }
  }

  func testArrayUninitializedIntrinsic<T>(_ x: T, _ y: T) -> Indirect<T> {
    return [x, y]
  }

  let (gradX, gradY) = pullback(at: Float(1), Float(1), in: {
    x, y in testArrayUninitializedIntrinsic(x, y)
  })(Indirect<Float>.TangentVector(x: 1))
  expectEqual(1, gradX)
  expectEqual(0, gradY)
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
      a: FloatArrayTan([1, 1]),
      b: FloatArrayTan([1, 0])),
    gradient(
      at: TwoArrays(a: [0, 0], b: [0, 0]),
      in: sumFirstThreeConcatted))
  expectEqual(
    TwoArrays.TangentVector(
      a: FloatArrayTan([1, 1, 1, 0]),
      b: FloatArrayTan([0, 0])),
    gradient(
      at: TwoArrays(a: [0, 0, 0, 0], b: [0, 0]),
      in: sumFirstThreeConcatted))
  expectEqual(
    TwoArrays.TangentVector(
      a: FloatArrayTan([]),
      b: FloatArrayTan([1, 1, 1, 0])),
    gradient(
      at: TwoArrays(a: [], b: [0, 0, 0, 0]),
      in: sumFirstThreeConcatted))
}

ArrayAutoDiffTests.testWithLeakChecking("Array.init(repeating:count:)") {
  @differentiable
  func repeating(_ x: Tracked<Float>) -> [Tracked<Float>] {
    Array(repeating: x, count: 10)
  }
  expectEqual(Tracked<Float>(10), gradient(at: .zero) { x in
    repeating(x).differentiableReduce(0, {$0 + $1}).value
  })
  expectEqual(Tracked<Float>(20), pullback(at: .zero, in: { x in
    repeating(x).differentiableReduce(0, {$0 + $1}).value
  })(2))
}

ArrayAutoDiffTests.test("Array.DifferentiableView.init") {
  @differentiable
  func constructView(_ x: [Float]) -> Array<Float>.DifferentiableView {
    return Array<Float>.DifferentiableView(x)
  }

  let backprop = pullback(at: [5, 6, 7, 8], in: constructView)
  expectEqual(
    FloatArrayTan([1, 2, 3, 4]),
    backprop(FloatArrayTan([1, 2, 3, 4])))
}

ArrayAutoDiffTests.test("Array.DifferentiableView.base") {
  @differentiable
  func accessBase(_ x: Array<Float>.DifferentiableView) -> [Float] {
    return x.base
  }

  let backprop = pullback(
    at: Array<Float>.DifferentiableView([5, 6, 7, 8]),
    in: accessBase)
  expectEqual(
    FloatArrayTan([1, 2, 3, 4]),
    backprop(FloatArrayTan([1, 2, 3, 4])))
}

ArrayAutoDiffTests.test("Array.DifferentiableView : KeyPathIterable") {
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

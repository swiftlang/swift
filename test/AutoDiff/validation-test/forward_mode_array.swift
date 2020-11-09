// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-forward-mode-differentiation)
// REQUIRES: executable_test

import StdlibUnittest
import DifferentiationUnittest

var ForwardModeTests = TestSuite("ForwardModeDifferentiation")

//===----------------------------------------------------------------------===//
// Array methods from ArrayDifferentiation.swift
//===----------------------------------------------------------------------===//

typealias FloatArrayTan = Array<Float>.TangentVector

ForwardModeTests.test("Array.+") {
  func sumFirstThreeConcatenating(_ a: [Float], _ b: [Float]) -> Float {
    let c = a + b
    return c[0] + c[1] + c[2]
  }

  expectEqual(3, differential(at: [0, 0], [0, 0], in: sumFirstThreeConcatenating)(.init([1, 1]), .init([1, 1])))
  expectEqual(0, differential(at: [0, 0], [0, 0], in: sumFirstThreeConcatenating)(.init([0, 0]), .init([0, 1])))
  expectEqual(1, differential(at: [0, 0], [0, 0], in: sumFirstThreeConcatenating)(.init([0, 1]), .init([0, 1])))
  expectEqual(1, differential(at: [0, 0], [0, 0], in: sumFirstThreeConcatenating)(.init([1, 0]), .init([0, 1])))
  expectEqual(1, differential(at: [0, 0], [0, 0], in: sumFirstThreeConcatenating)(.init([0, 0]), .init([1, 1])))
  expectEqual(2, differential(at: [0, 0], [0, 0], in: sumFirstThreeConcatenating)(.init([1, 1]), .init([0, 1])))

  expectEqual(
    3,
    differential(at: [0, 0, 0, 0], [0, 0], in: sumFirstThreeConcatenating)(.init([1, 1, 1, 1]), .init([1, 1])))
  expectEqual(
    3,
    differential(at: [0, 0, 0, 0], [0, 0], in: sumFirstThreeConcatenating)(.init([1, 1, 1, 0]), .init([0, 0])))

  expectEqual(
    3,
    differential(at: [], [0, 0, 0, 0], in: sumFirstThreeConcatenating)(.init([]), .init([1, 1, 1, 1])))
  expectEqual(
    0,
    differential(at: [], [0, 0, 0, 0], in: sumFirstThreeConcatenating)(.init([]), .init([0, 0, 0, 1])))
}

ForwardModeTests.test("Array.init(repeating:count:)") {
  @differentiable
  func repeating(_ x: Float) -> [Float] {
    Array(repeating: x, count: 10)
  }
  expectEqual(Float(10), derivative(at: .zero) { x in
    repeating(x).differentiableReduce(0, {$0 + $1})
  })
  expectEqual(Float(20), differential(at: .zero, in: { x in
    repeating(x).differentiableReduce(0, {$0 + $1})
  })(2))
}

ForwardModeTests.test("Array.DifferentiableView.init") {
  @differentiable
  func constructView(_ x: [Float]) -> Array<Float>.DifferentiableView {
    return Array<Float>.DifferentiableView(x)
  }

  let forward = differential(at: [5, 6, 7, 8], in: constructView)
  expectEqual(
    FloatArrayTan([1, 2, 3, 4]),
    forward(FloatArrayTan([1, 2, 3, 4])))
}

ForwardModeTests.test("Array.DifferentiableView.base") {
  @differentiable
  func accessBase(_ x: Array<Float>.DifferentiableView) -> [Float] {
    return x.base
  }

  let forward = differential(
    at: Array<Float>.DifferentiableView([5, 6, 7, 8]),
    in: accessBase)
  expectEqual(
    FloatArrayTan([1, 2, 3, 4]),
    forward(FloatArrayTan([1, 2, 3, 4])))
}

ForwardModeTests.test("Array.differentiableMap") {
  let x: [Float] = [1, 2, 3]
  let tan = Array<Float>.TangentVector([1, 1, 1])

  func multiplyMap(_ a: [Float]) -> [Float] {
    return a.differentiableMap({ x in 3 * x })
  }
  expectEqual([3, 3, 3], differential(at: x, in: multiplyMap)(tan))

  func squareMap(_ a: [Float]) -> [Float] {
    return a.differentiableMap({ x in x * x })
  }
  expectEqual([2, 4, 6], differential(at: x, in: squareMap)(tan))
}

ForwardModeTests.test("Array.differentiableReduce") {
  let x: [Float] = [1, 2, 3]
  let tan = Array<Float>.TangentVector([1, 1, 1])

  func sumReduce(_ a: [Float]) -> Float {
    return a.differentiableReduce(0, { $0 + $1 })
  }
  expectEqual(1 + 1 + 1, differential(at: x, in: sumReduce)(tan))

  func productReduce(_ a: [Float]) -> Float {
    return a.differentiableReduce(1, { $0 * $1 })
  }
  expectEqual(x[1] * x[2] + x[0] * x[2] + x[0] * x[1], differential(at: x, in: productReduce)(tan))

  func sumOfSquaresReduce(_ a: [Float]) -> Float {
    return a.differentiableReduce(0, { $0 + $1 * $1 })
  }
  expectEqual(2 * x[0] + 2 * x[1] + 2 * x[2], differential(at: x, in: sumOfSquaresReduce)(tan))
}

runAllTests()

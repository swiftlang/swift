// RUN: %target-run-simple-swift
// REQUIRES: executable_test
//
// Tensor API tests.

import TensorFlow
import StdlibUnittest

var TensorTests = TestSuite("Tensor")

@inline(never)
func testInitializers() {
  let x = Tensor([[1.0, 2.0, 3.0], [2.0, 4.0, 6.0]])
  expectEqual(Array(x.array.units),
    [1.0, 2.0, 3.0, 2.0, 4.0, 6.0])
}
TensorTests.test("Initializers", testInitializers)

@inline(never)
func testFactoryInitializers() {
  let x = Tensor<Float>.ones(shape: [1, 10])
  expectEqual(Array(x.array.units),
    Array(repeating: 1, count: 10))
}
TensorTests.test("FactoryInitializers", testFactoryInitializers)

@inline(never)
func testSimpleMath() {
  let x = Tensor<Float>([1.2, 1.2]).toDevice()
  let y = tanh(x)
  _ = y
  // TODO: Check result. Currently crashing because of rank/shape getters
  // expectTrue(y.array - 0.833655 < 0.000001)
}
TensorTests.test("SimpleMath", testSimpleMath)

@inline(never)
func testMultiOpMath() {
  let x = Tensor<Float>([1.2, 1.2]).toDevice()
  let y = Tensor<Float>([4.3, 4.3]).toDevice()
  let sum = x + y
  let squared = sum * sum
  let expsqr = exp(squared)
  _ = expsqr
  // TODO: Check result
}
TensorTests.test("testMultiOpMath", testMultiOpMath)

@inline(never)
func testXWPlusB() {
  // Shape: 4
  let x = Tensor([1.0, 2.0, 2.0, 1.0]).toDevice()
  // Shape: 2 x 4
  let w = Tensor([[1.0, 0.0], [3.0, 0.0], [2.0, 3.0], [1.0, 0.0]]).toDevice()
  // Shape: 2
  let b = Tensor([0.5, 0.5]).toDevice()
  // Do xW+b!
  _ = x ⊗ w + b
  // TODO: Check result
}
TensorTests.test("testXWPlusB", testXWPlusB)

@inline(never)
func testXORInference() {
  func xor(_ x: Double, _ y: Double) -> Double {
    // FIXME: If params are declared outside of `xor`, it would crash.
    // 2 x 4
    let w1 = Tensor([[-1.83586664, -0.20809225, 0.47667537, 1.90780607],
                     [-1.83523219, -0.51167348, 0.15490439, 1.91018065]]).toDevice()
    // 1 x 4
    let b1 = Tensor([[2.54353216, 0.25132703, -0.16503136, -0.85754058]]).toDevice()
    // 4 x 1
    let w2 = Tensor([[ 3.04350065], [ 0.35590511], [-0.3252157 ], [ 3.49349223]]).toDevice()
    // 1 x 1
    let b2 = Tensor([[-0.74635993]]).toDevice()

    let x = Tensor([[x, y]]).toDevice()
    let o1 = tanh(x ⊗ w1 + b1)
    let y = tanh(o1 ⊗ w2 + b2)
    return y.array.units[0] // TODO: use better scalar getter
  }
  expectLT(abs(xor(0.0, 0.0) - 0.0), 0.1)
  expectLT(abs(xor(0.0, 1.0) - 1.0), 0.1)
  expectLT(abs(xor(1.0, 0.0) - 1.0), 0.1)
  expectLT(abs(xor(1.0, 1.0) - 0.0), 0.1)
}
TensorTests.test("XORInference", testXORInference)

TensorTests.test("MLPClassifierStruct") {
  struct MLPClassifier {
    // 2 x 4
    var w1 = Tensor<Float>([[1.0, 0.8, 0.4, 0.4],
        [0.4, 0.3, 0.2, 0.1]]).toDevice()
    // 4 x 1
    var w2 = Tensor<Float>([[0.4],
        [0.4],
        [0.3],
        [0.9]]).toDevice()
    var b1 = Tensor<Float>.zeros(shape: [1, 4]).toDevice()
    var b2 = Tensor<Float>.zeros(shape: [1, 1]).toDevice()

    @_versioned
    @_inlineable
    func prediction(for x: Tensor<Float>) -> Tensor<Float> {
      let o1 = tanh(x ⊗ w1 + b1)
      return tanh(o1 ⊗ w2 + b2)
    }
  }
  // TODO: Check results
  _ = MLPClassifier().prediction(for: Tensor([[1, 0.5]]).toDevice())
}

@inline(never)
func testRankGetter() {
  let x = Tensor([[1.0, 2.0, 3.0], [4.0, 5.0, 6.0]])
  expectEqual(x.rank, 2)
}
TensorTests.test("RankGetter", testRankGetter)

// TODO: Merge into the previous example when we support code motion to avoid
// sends.
@inline(never)
func testRankGetter2() {
  let y: Tensor<Int> = .ones(shape: [1, 2, 2, 2, 2, 2, 1])
  expectEqual(y.rank, 7)
}
TensorTests.test("RankGetter2", testRankGetter2)

@inline(never)
func testShapeGetter() {
  let x = Tensor([[1.0, 2.0, 3.0], [4.0, 5.0, 6.0]])
  expectEqual(x.shape, [2, 3])
}
TensorTests.test("ShapeGetter", testShapeGetter)

@inline(never)
func testShapeGetter2() {
  let y: Tensor<Int> = .ones(shape: [1, 2, 2, 2, 2, 2, 1])
  expectEqual(y.shape, [1, 2, 2, 2, 2, 2, 1])
}
TensorTests.test("ShapeGetter2", testShapeGetter2)

runAllTests()

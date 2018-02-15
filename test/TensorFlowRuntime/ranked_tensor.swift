// RUN: %target-run-simple-swift
// REQUIRES: executable_test
//
// Tensor API tests.

import TensorFlow
import TestUtils
import StdlibUnittest

var RankedTensorTests = TestSuite("RankedTensor")

/// Determines if two floating point numbers are very nearly equal.
func expectNearlyEqual<T : FloatingPoint & ExpressibleByFloatLiteral>(
  _ lhs: T, _ rhs: T, byError error: T = 0.000001
) {
  expectLT(abs(lhs - rhs), error)
}

/// Determines if two collections of floating point numbers are very nearly
/// equal.
func expectPointwiseNearlyEqual<T, C1, C2>(
  _ lhs: C1, _ rhs: C2, byError error: T = 0.000001
) where T : FloatingPoint & ExpressibleByFloatLiteral,
  C1 : Collection, C2 : Collection, C1.Element == T, C1.Element == C2.Element {
  precondition(lhs.count == rhs.count, "Scalar count mismatch.")
  for (l, r) in zip(lhs, rhs) {
    expectNearlyEqual(l, r, byError: error)
  }
}

RankedTensorTests.testCPUAndGPU("Initializers") {
  let vector = Tensor1D([1.0, 2.0, 3.0, 4.0, 5.0, 6.0])
  let matrix = Tensor2D([[1.0, 2.0, 3.0], [4.0, 5.0, 6.0]])
  let tensor = Tensor3D(identicallyRanked: Tensor<Float>(ones: [2, 3, 4]))
  expectEqual([1, 2, 3, 4, 5, 6], vector.array)
  expectEqual(Array2D(shape: [2, 3], scalars: [1, 2, 3, 4, 5, 6]), matrix.array)
  expectEqual(Array3D(shape: [2, 3, 4], repeating: 1), tensor.array)
}

RankedTensorTests.testCPUAndGPU("FactoryInitializers") {
  let x = Tensor2D<Float>(ones: [1, 10])
  expectEqual([1, 10], x.shape)
  expectEqual(Array(repeating: 1, count: 10), x.scalars)
}

RankedTensorTests.testCPUAndGPU("RandomInitializer") {
  let random = Tensor2D<Float>(
    randomNormal: [3, 4], mean: 100, stddev: 50, seed: 42
  )
  expectEqual([3, 4], random.shape)
  expectPointwiseNearlyEqual([
      137.281219, 68.1401749, 102.428467, 67.4076538, 56.9186516, 100.973923,
      107.604424, 150.683273, 195.382324, 22.3883247, 55.4706612, 118.716873
  ], random.scalars)
}

RankedTensorTests.testCPUAndGPU("ScalarToTensorConversion1") {
  let matrix = -1.makeTensor2D()
  expectEqual([1, 1], matrix.shape)
  expectEqual([-1], matrix.scalars)
}

// TODO: Merge into the previous example when we support code motion to avoid
// sends.
RankedTensorTests.testCPUAndGPU("ScalarToTensorConversion2") {
  let tensor = 42.makeTensor4D()
  expectEqual([1, 1, 1, 1], tensor.shape)
  expectEqual([42], tensor.scalars)
}

RankedTensorTests.testCPUAndGPU("ArrayXDConversion") {
  let array3D = Array3D(shape: [2, 3, 4], repeating: 1.0)
  let tensor3D = Tensor3D(array3D)
  expectEqual(array3D, tensor3D.array)
}

RankedTensorTests.testCPUAndGPU("DataTypeCast") {
  let x = Tensor2D<Int32>(ones: [5, 5])
  let ints = Tensor2D<Int64>(x)
  let floats = Tensor2D<Float>(x)
  let i8s = Tensor2D<Int8>(floats)
  expectEqual(Array2D(shape: [5, 5], repeating: 1), ints.array)
  expectEqual(Array2D(shape: [5, 5], repeating: 1), floats.array)
  expectEqual(Array2D(shape: [5, 5], repeating: 1), i8s.array)
}

RankedTensorTests.testCPUAndGPU("BoolToNumericCast") {
  let bools = Tensor2D<Bool>([[true, false], [true, false]])
  let ints = Tensor2D<Int64>(bools)
  let floats = Tensor2D<Float>(bools)
  let i8s = Tensor2D<Int8>(bools)
  expectEqual(Array2D(shape: [2, 2], scalars: [1, 0, 1, 0]), ints.array)
  expectEqual(Array2D(shape: [2, 2], scalars: [1, 0, 1, 0]), floats.array)
  expectEqual(Array2D(shape: [2, 2], scalars: [1, 0, 1, 0]), i8s.array)
}

RankedTensorTests.testCPUAndGPU("Reduction") {
  let x = Tensor2D<Float>([[1, 2, 3, 4, 5], [1, 2, 3, 4, 5]])
  let sum = x.sum(alongAxis: 0)
  expectEqual([2, 4, 6, 8, 10], sum.array)
}

RankedTensorTests.testCPUAndGPU("SimpleMath") {
  let x = Tensor1D<Float>([1.2, 1.2])
  let y = tanh(x)
  expectPointwiseNearlyEqual([0.833655, 0.833655], y.array, byError: 0.0001)
}

RankedTensorTests.testCPUAndGPU("Convolution") {
  // TODO: the code for initializing Tensor4D instances here is quite verbose.
  // Consider adding `init(shape:repeating)` and/or `init(shape:scalars)`
  // initializers to TensorXD?
  let x = Tensor4D(
    identicallyRanked: Tensor<Float>(shape: [1, 1, 3, 3], repeating: 0.5)
  )
  let filter = Tensor4D(
    identicallyRanked: Tensor<Float>(
      shape: [1, 1, 3, 3], scalars: [0, 1, 0, 1, 1, 1, 0, 1, 0]
    )
  )
  let y = x.convolved2D(withFilter: filter, strides: [1, 1, 1, 1],
                        padding: .same)
  expectEqual(Array4D(shape: [1, 1, 3, 3],
                      scalars: [0.5, 1.5, 0.5,
                                0.5, 1.5, 0.5,
                                0.5, 1.5, 0.5]),
              y.array)
}

RankedTensorTests.testCPUAndGPU("3Adds") {
  let a = Tensor1D([1])
  let b = Tensor1D([2])
  let c = Tensor1D([3])
  let o = a + b + c

  expectEqual([6], o.array)
}

RankedTensorTests.testCPUAndGPU("testMultiOpMath") {
  let x = Tensor1D<Float>([1.2, 1.2])
  let y = Tensor1D<Float>([2.4, 2.4])
  let t1 = x + y
  let t2 = t1 * t1
  let t3 = sqrt(t2)

  expectPointwiseNearlyEqual([3.6, 3.6], t1.array)
  expectPointwiseNearlyEqual([12.96, 12.96], t2.array)
  expectPointwiseNearlyEqual([3.6, 3.6], t3.array)
}

RankedTensorTests.testCPUAndGPU("testXWPlusB") {
  // Shape: 1 x 4
  let x = Tensor2D([[1.0, 2.0, 2.0, 1.0]])
  // Shape: 4 x 2
  let w = Tensor2D([[1.0, 0.0], [3.0, 0.0], [2.0, 3.0], [1.0, 0.0]])
  // Shape: 2
  let b = Tensor1D([0.5, 0.5])
  // Do xW+b!
  let result = x ⊗ w + b.rankLifted()
  expectEqual([1, 2], result.shape)
  expectPointwiseNearlyEqual([12.5, 6.5], result.scalars)
}

RankedTensorTests.testCPUAndGPU("Transpose") {
  // Shape: 3 x 2
  let xT = Tensor2D([[1, 2], [3, 4], [5, 6]]).transposed()
  expectEqual(2, xT.rank)
  expectEqual([2, 3], xT.shape)
  expectEqual([1, 3, 5, 2, 4, 6], xT.scalars)
}

// FIXME: Partitioner bug (b/72997202)
#if false // Remove #if when fixed.
// FIXME: The While op doesn't work on the CPU.
RankedTensorTests.testGPU("simpleCounterLoop") {
  let maxCount = 100
  var a = Tensor1D<Int32>(0)
  let b = Tensor1D<Int32>(1)
  var count = 0

  a -= b
  while count < maxCount {
    a += b
    count += 1
  }
  a -= b
  expectEqual([8], a.scalars)
}
#endif

@inline(never)
func testXORInference() {
  func xor(_ x: Double, _ y: Double) -> Double {
    let x = Tensor2D([[x, y]])

    // FIXME: If params are declared outside of `xor`, it would crash.
    // 2 x 4
    let w1 = Tensor2D(
      [[-1.83586664, -0.20809225, 0.47667537, 1.90780607],
       [-1.83523219, -0.51167348, 0.15490439, 1.91018065]])
    // 1 x 4
    let b1 = Tensor2D(
      [[2.54353216, 0.25132703, -0.16503136, -0.85754058]])
    // 4 x 1
    let w2 = Tensor2D(
      [[3.04350065], [0.35590511], [-0.3252157], [3.49349223]])
    // 1 x 1
    let b2 = Tensor2D([[-0.74635993]])

    let o1 = tanh(x ⊗ w1 + b1)
    let y = tanh(o1 ⊗ w2 + b2)
    return y.scalars[0]
  }
  expectNearlyEqual(0.0, xor(0.0, 0.0), byError: 0.1)
  expectNearlyEqual(1.0, xor(0.0, 1.0), byError: 0.1)
  expectNearlyEqual(1.0, xor(1.0, 0.0), byError: 0.1)
  expectNearlyEqual(0.0, xor(1.0, 1.0), byError: 0.1)
}
RankedTensorTests.testCPUAndGPU("XORInference", testXORInference)

runAllTests()

// RUN: %target-run-simple-swift
// REQUIRES: executable_test
//
// Tensor API tests.

import TensorFlow
import StdlibUnittest

extension TestSuite {
  func testCPUAndGPU(_ name: String, _ body: @escaping () -> Void) {
    test(name + "_CPU") {
      _RuntimeConfig.runsOnGPU = false
      body()
    }
    testGPU(name, body)
  }
  func testGPU(_ name: String, _ body: @escaping () -> Void) {
#if CUDA
    test(name + "_GPU") {
      _RuntimeConfig.runsOnGPU = true
      body()
    }
#endif
  }
}

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
  precondition(lhs.count == rhs.count, "Unit count mismatch.")
  for (l, r) in zip(lhs, rhs) {
    expectNearlyEqual(l, r, byError: error)
  }
}

RankedTensorTests.testCPUAndGPU("Initializers") {
  let x = Tensor1D([1.0, 2.0, 3.0, 2.0, 4.0, 6.0])
  expectEqual([1.0, 2.0, 3.0, 2.0, 4.0, 6.0], x.units)
}

RankedTensorTests.testCPUAndGPU("FactoryInitializers") {
  let x = Tensor2D<Float>.ones(shape: [1, 10])
  expectEqual([1, 10], x.shape)
  expectEqual(Array(repeating: 1, count: 10), x.units)
}

RankedTensorTests.testCPUAndGPU("DataTypeCast") {
  let x: Tensor2D<Int32> = .ones(shape: [5, 5])
  let ints = Tensor2D<Int>(x)
  let floats = Tensor2D<Float>(x)
  let i8s = Tensor2D<Int8>(floats)
  /// TODO(danielzheng): compare `.array` instead after it is implemented.
  // expectEqual([5, 5], ints.shape)
  // expectEqual([5, 5], floats.shape)
  // expectEqual([5, 5], i8s.shape)
  expectEqual(Array(repeating: 1, count: 25), ints.units)
  expectEqual(Array(repeating: 1, count: 25), floats.units)
  expectEqual(Array(repeating: 1, count: 25), i8s.units)
}

RankedTensorTests.testCPUAndGPU("Reduction") {
  let x = Tensor2D<Float>([[1, 2, 3, 4, 5], [1, 2, 3, 4, 5]])
  let sum = x.sum(alongAxis: 0)
  expectEqual([5], sum.shape)
  expectEqual([2, 4, 6, 8, 10], sum.units)
}

RankedTensorTests.testCPUAndGPU("SimpleMath") {
  let x = Tensor1D<Float>([1.2, 1.2])
  let y = tanh(x)
  expectEqual([2], y.shape)
  expectPointwiseNearlyEqual([0.833655, 0.833655], y.units, byError: 0.0001)
}

#if false
RankedTensorTests.testCPUAndGPU("Convolution") {
  let x = Tensor4D<Float>(shape: [1, 3, 3, 1], repeating: 0.5)
  let filter = Tensor4D<Float>(shape: [1, 3, 3, 1],
                               units: [0, 1, 0, 1, 1, 1, 0, 1, 0])
  let y = x.convolved2D(withFilter: filter,
                        strides: [1, 1, 1, 1], padding: .same)
  expectEqual([0.5, 1.5, 0.5,
               0.5, 1.5, 0.5,
               0.5, 1.5, 0.5],
              y.units)
}
#endif

RankedTensorTests.testCPUAndGPU("3Adds") {
  let a = Tensor1D([1])
  let b = Tensor1D([2])
  let c = Tensor1D([3])
  let o = a + b + c

  expectEqual([1], o.shape)
  expectEqual([6], o.units)
}

RankedTensorTests.testCPUAndGPU("testMultiOpMath") {
  let x = Tensor1D<Float>([1.2, 1.2])
  let y = Tensor1D<Float>([2.4, 2.4])
  let sum = x + y
  let squared = sum * sum
  let squareRooted = sqrt(squared)

  // expectEqual([2], sum.shape)
  // expectEqual([2], squared.shape)
  // expectEqual([2], squareRooted.shape)
  expectPointwiseNearlyEqual([3.6, 3.6], sum.units)
  expectPointwiseNearlyEqual([12.96, 12.96], squared.units)
  expectPointwiseNearlyEqual([3.6, 3.6], squareRooted.units)
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
  expectPointwiseNearlyEqual([12.5, 6.5], result.units)
}

RankedTensorTests.testCPUAndGPU("Transpose") {
  // Shape: 3 x 2
  let xT = Tensor2D([[1, 2], [3, 4], [5, 6]]).transposed()
  expectEqual(2, xT.rank)
  expectEqual([2, 3], xT.shape)
  expectEqual([1, 3, 5, 2, 4, 6], xT.units)
}

// FIXME: The While op doesn't work on the CPU.
RankedTensorTests.testGPU("simpleCounterLoop") {
  let maxCount = 100
  var a = Tensor1D<Int>(0)
  let b = Tensor1D<Int>(1)
  var count = 0

  a -= b
  while count < maxCount {
    a += b
    count += 1
  }
  a -= b
  expectEqual([8], a.units)
}

@inline(never)
func testXORInference() {
  func xor(_ x: Double, _ y: Double) -> Double {
    // FIXME: If params are declared outside of `xor`, it would crash.
    // 2 x 4
    let w1 = Tensor2D(
      [[-1.83586664, -0.20809225, 0.47667537, 1.90780607],
       [-1.83523219, -0.51167348, 0.15490439, 1.91018065]]).toDevice()
    // 1 x 4
    let b1 = Tensor2D(
      [[2.54353216, 0.25132703, -0.16503136, -0.85754058]]).toDevice()
    // 4 x 1
    let w2 = Tensor2D(
      [[3.04350065], [0.35590511], [-0.3252157], [3.49349223]]).toDevice()
    // 1 x 1
    let b2 = Tensor2D([[-0.74635993]]).toDevice()

    let x = Tensor2D([[x, y]]).toDevice()
    let o1 = tanh(x ⊗ w1 + b1)
    let y = tanh(o1 ⊗ w2 + b2)
    return y.units[0]
  }
  expectNearlyEqual(0.0, xor(0.0, 0.0), byError: 0.1)
  expectNearlyEqual(1.0, xor(0.0, 1.0), byError: 0.1)
  expectNearlyEqual(1.0, xor(1.0, 0.0), byError: 0.1)
  expectNearlyEqual(0.0, xor(1.0, 1.0), byError: 0.1)
}
RankedTensorTests.testCPUAndGPU("XORInference", testXORInference)

runAllTests()

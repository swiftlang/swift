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
  let vector = Tensor1D([1.0, 2.0, 3.0, 2.0, 4.0, 6.0])
  expectEqual(vector.units, [1.0, 2.0, 3.0, 2.0, 4.0, 6.0])
}

RankedTensorTests.testCPUAndGPU("FactoryInitializers") {
  let x = Tensor2D<Float>.ones(shape: [1, 10])
  // expectEqual(x.shape, (1, 10))
  expectEqual(x.units, Array(repeating: 1, count: 10))
}

RankedTensorTests.testCPUAndGPU("DataTypeCast") {
  let x: Tensor2D<Int32> = .ones(shape: [5, 5])
  let ints = Tensor2D<Int>(x)
  let floats = Tensor2D<Float>(x)
  let i8s = Tensor2D<Int8>(floats)
  /// TODO(danielzheng): compare `.array` instead after it is implemented.
  // expectEqual(ints.shape, (5, 5))
  // expectEqual(floats.shape, (5, 5))
  // expectEqual(i8s.shape, (5, 5))
  expectEqual(ints.units, Array(repeating: 1, count: 25))
  expectEqual(floats.units, Array(repeating: 1, count: 25))
  expectEqual(i8s.units, Array(repeating: 1, count: 25))
}

RankedTensorTests.testCPUAndGPU("Reduction") {
  let x = Tensor2D<Float>([[1, 2, 3, 4, 5], [1, 2, 3, 4, 5]])
  let sum = x.sum(alongAxis: 0)
  // expectEqual(sum.shape, 5)
  expectEqual(sum.units, [2, 4, 6, 8, 10])
}

RankedTensorTests.testCPUAndGPU("SimpleMath") {
  let x = Tensor1D<Float>([1.2, 1.2])
  let y = tanh(x)
  // expectEqual(y.shape, 2)
  let units = y.units
  expectPointwiseNearlyEqual(units, [0.833655, 0.833655], byError: 0.0001)
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

  // expectEqual(o.shape, 1)
  expectEqual(o.units, [6])
}

RankedTensorTests.testCPUAndGPU("testMultiOpMath") {
  let x = Tensor1D<Float>([1.2, 1.2])
  let y = Tensor1D<Float>([2.4, 2.4])
  let sum = x + y
  let squared = sum * sum
  let squareRooted = sqrt(squared)

  // expectEqual(sum.shape, 2)
  // expectEqual(squared.shape, 2)
  // expectEqual(expsqrt.shape, 2)
  let sumUnits = sum.units
  let squaredUnits = squared.units
  let squareRootedUnits = squareRooted.units
  expectPointwiseNearlyEqual(sumUnits, [3.6, 3.6])
  expectPointwiseNearlyEqual(squaredUnits, [12.96, 12.96])
  expectPointwiseNearlyEqual(squareRootedUnits, [3.6, 3.6])
}

RankedTensorTests.testCPUAndGPU("testXWPlusB") {
  // Shape: 4
  let x = Tensor1D([1.0, 2.0, 2.0, 1.0])
  // Shape: 2 x 4
  let w = Tensor2D([[1.0, 0.0], [3.0, 0.0], [2.0, 3.0], [1.0, 0.0]])
  // Shape: 2
  let b = Tensor1D([0.5, 0.5])
  // Do xW+b!
  // TODO: fix `transposed()`/`transposed(withPermutations:)`
  // let result = x.rankLifted() ⊗ w.transposed() + b.rankLifted()
  // expectPointwiseNearlyEqual(result.units, [12.5, 6.5])
}

// FIXME: The While op doesn't work on the CPU.
RankedTensorTests.testGPU("simpleCounterLoop") {
  let maxCount = 100
  var a = Tensor1D<Int>(0)
  let b = Tensor1D<Int>(1)

  a -= b

  var count = 0
  while count < maxCount {
    a += b
    count += 1
  }
  a -= b
  expectEqual(a.units, [8])
  expectEqual(a.units, [8])
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
  expectNearlyEqual(xor(0.0, 0.0), 0.0, byError: 0.1)
  expectNearlyEqual(xor(0.0, 1.0), 1.0, byError: 0.1)
  expectNearlyEqual(xor(1.0, 0.0), 1.0, byError: 0.1)
  expectNearlyEqual(xor(1.0, 1.0), 0.0, byError: 0.1)
}
RankedTensorTests.testCPUAndGPU("XORInference", testXORInference)

runAllTests()

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

var TensorTests = TestSuite("Tensor")

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

TensorTests.testCPUAndGPU("Initializers") {
  let x = Tensor([[1.0, 2.0, 3.0], [2.0, 4.0, 6.0]])
  expectEqual([1.0, 2.0, 3.0, 2.0, 4.0, 6.0], x.scalars)
}

TensorTests.testCPUAndGPU("FactoryInitializers") {
  let x = Tensor<Float>.ones(shape: [1, 10])
  expectEqual([1, 10], x.shape)
  expectEqual(Array(repeating: 1, count: 10), x.scalars)
}

TensorTests.testCPUAndGPU("DataTypeCast") {
  let x = Tensor<Int32>.ones(shape: [5, 5])
  let ints = Tensor<Int>(x)
  let floats = Tensor<Float>(x)
  let i8s = Tensor<Int8>(floats)
  expectEqual(ShapedArray(shape: [5, 5], repeating: 1), ints.array)
  expectEqual(ShapedArray(shape: [5, 5], repeating: 1), floats.array)
  expectEqual(ShapedArray(shape: [5, 5], repeating: 1), i8s.array)
}

TensorTests.testCPUAndGPU("Reduction") {
  let x = Tensor<Float>([[1, 2, 3, 4, 5], [1, 2, 3, 4, 5]])
  let sum = x.sum(alongAxes: [0], keepingDimensions: true)
  expectEqual(ShapedArray(shape: [1, 5], scalars: [2, 4, 6, 8, 10]), sum.array)
}

TensorTests.testCPUAndGPU("SimpleMath") {
  let x = Tensor<Float>([1.2, 1.2])
  let y = tanh(x)
  let array = y.array
  expectPointwiseNearlyEqual([0.833655, 0.833655], array.scalars, byError: 0.0001)
}

TensorTests.testCPUAndGPU("Convolution") {
  let x = Tensor<Float>(shape: [1, 1, 3, 3], repeating: 0.5)
  let filter = Tensor<Float>(shape: [1, 1, 3, 3],
                             scalars: [0, 1, 0, 1, 1, 1, 0, 1, 0])
  let y = x.convolved2D(withFilter: filter,
                        strides: [1, 1, 1, 1], padding: .same)
  expectEqual(ShapedArray(shape: [1, 1, 3, 3],
                          scalars: [0.5, 1.5, 0.5,
                                  0.5, 1.5, 0.5,
                                  0.5, 1.5, 0.5]),
              y.array)
}

TensorTests.testCPUAndGPU("3Adds") {
  let a = Tensor([1])
  let b = Tensor([2])
  let c = Tensor([3])

  let o = a + b + c
  expectEqual([6], o.scalars)
}

TensorTests.testCPUAndGPU("MultiOpMath") {
  let x = Tensor<Float>([1.2, 1.2])
  let y = Tensor<Float>([2.4, 2.4])
  let sum = x + y
  let squared = sum * sum
  let squareRooted = sqrt(squared)

  // expectEqual([2], sum.shape)
  // expectEqual([2], squared.shape)
  // expectEqual([2], squareRooted.shape)
  expectPointwiseNearlyEqual([3.6, 3.6], sum.scalars)
  expectPointwiseNearlyEqual([12.96, 12.96], squared.scalars)
  expectPointwiseNearlyEqual([3.6, 3.6], squareRooted.scalars)
}

TensorTests.testCPUAndGPU("XWPlusB") {
  // Shape: 1 x 4
  let x = Tensor([[1.0, 2.0, 2.0, 1.0]]).toDevice()
  // Shape: 4 x 2
  let w = Tensor([[1.0, 0.0], [3.0, 0.0], [2.0, 3.0], [1.0, 0.0]]).toDevice()
  // Shape: 2
  let b = Tensor([0.5, 0.5])
  // Do xW+b!
  let result = x ⊗ w + b
  expectEqual([1, 2], result.shape)
  expectEqual([12.5, 6.5], result.scalars)
}

TensorTests.testCPUAndGPU("Transpose") {
  // Shape: 3 x 2
  let xT = Tensor([[1, 2], [3, 4], [5, 6]]).transposed()
  let xTArray = xT.array
  expectEqual(2, xTArray.rank)
  expectEqual([2, 3], xTArray.shape)
  expectEqual([1, 3, 5, 2, 4, 6], xTArray.scalars)
}

// FIXME: The While op doesn't work on the CPU.
TensorTests.testGPU("simpleCounterLoop") {
  let maxCount = 100
  var a = Tensor<Int>(0)
  let b = Tensor<Int>(1)
  var count = 0

  a -= b
  while count < maxCount {
    a += b
    count += 1
  }
  a -= b
  expectEqual(8, a.scalar)
}

// FIXME: Partitioner bug (b/72997202)
#if false // Remove #if when fixed.
// This is derived from a TF Eager testcase.
TensorTests.testGPU("loopsAndConditions") {
  var a = Tensor<Int>(6)
  var count = Tensor<Int>(0)
  while (a != 1).scalar! {
    if (a % 2 == 0).scalar! {
      a = a / 2
    } else {
      a = 3 * a + 1
    }
    count += 1
  }
  expectEqual(8, count.scalar)
}
#endif

@inline(never)
func testXORInference() {
  func xor(_ x: Double, _ y: Double) -> Double {
    // FIXME: If params are declared outside of `xor`, it would crash.
    // 2 x 4
    let w1 = Tensor(
      [[-1.83586664, -0.20809225, 0.47667537, 1.90780607],
       [-1.83523219, -0.51167348, 0.15490439, 1.91018065]]).toDevice()
    // 1 x 4
    let b1 = Tensor(
      [[2.54353216, 0.25132703, -0.16503136, -0.85754058]]).toDevice()
    // 4 x 1
    let w2 = Tensor(
      [[3.04350065], [0.35590511], [-0.3252157], [3.49349223]]).toDevice()
    // 1 x 1
    let b2 = Tensor([[-0.74635993]]).toDevice()

    let x = Tensor([[x, y]]).toDevice()
    let o1 = tanh(x ⊗ w1 + b1)
    let y = tanh(o1 ⊗ w2 + b2)
    return y.array.scalars[0] // TODO: use better scalar getter
  }
  expectNearlyEqual(0.0, xor(0.0, 0.0), byError: 0.1)
  expectNearlyEqual(1.0, xor(0.0, 1.0), byError: 0.1)
  expectNearlyEqual(1.0, xor(1.0, 0.0), byError: 0.1)
  expectNearlyEqual(0.0, xor(1.0, 1.0), byError: 0.1)
}
TensorTests.testCPUAndGPU("XORInference", testXORInference)

TensorTests.testCPUAndGPU("MLPClassifierStruct") {
  struct MLPClassifier {
    // 2 x 4
    var w1 = Tensor<Float>([[1.0, 0.8, 0.4, 0.4],
                            [0.4, 0.3, 0.2, 0.1]]).toDevice()
    // 4 x 1
    var w2 = Tensor<Float>([[0.4],
                            [0.4],
                            [0.3],
                            [0.9]]).toDevice()
    var b1 = Tensor<Float>.zeros(shape: [1, 4])
    var b2 = Tensor<Float>.zeros(shape: [1, 1])

    /// - NOTE: This initializer must be manually declared, because the initializer
    /// logic for the variables declared above is large, and we need to mark
    /// this as inline(__always).
    /// - TODO: Remove when deabstraction is implemented.
    @inline(__always)
    init() {}

    @inline(__always)
    func prediction(for x: Tensor<Float>) -> Tensor<Float> {
      let o1 = tanh(x ⊗ w1 + b1)
      return tanh(o1 ⊗ w2 + b2)
    }
  }
  let predictFor = Tensor<Float>([[1, 0.5]]).toDevice()
  let classifier = MLPClassifier()
  let _ = classifier.prediction(for: predictFor)
  // TODO: Check result.
}

TensorTests.testCPUAndGPU("Reshape") {
  let x = Tensor([[1], [2], [3]]).toDevice() // Shape 3 x 1
  let y = x.reshaped([1, 3, 1, 1, 1])
  expectEqual([1, 3, 1, 1, 1], y.shape)
}

TensorTests.testCPUAndGPU("ReshapeScalar") {
  let z = Tensor([[10]]).toDevice().reshaped([])
  expectEqual([], z.shape)
}

TensorTests.testGPU("StraightLineXORTraining") {
  // Hyper-parameters
  let iterationCount = 1000
  let learningRate: Float = 0.2
  var loss = Float.infinity

  // Parameters
  var w1 = Tensor<Float>(shape: [2, 4], repeating: 0.5)
  var w2 = Tensor<Float>(shape: [4, 1], repeating: 0.5)
  var b1 = Tensor<Float>(shape: [1, 4], repeating: 0.0)
  var b2 = Tensor<Float>(shape: [1, 1], repeating: 0.0)

  // Training data
  let x = Tensor<Float>(
    shape: [4, 2],
    scalars: [0.0, 0.0, 0.0, 1.0, 1.0, 0.0, 1.0, 1.0]
  )
  let y = Tensor<Float>(
    shape: [4, 1],
    scalars: [0.0, 1.0, 1.0, 0.0]
  )

  // Training loop
  // FIXME: Partitioner bug (b/72997202)
  // Uncomment for loop when fixed.
  // for _ in 0..<iterationCount {
    let z1 = x.dot(w1) + b1
    let h1 = sigmoid(z1)
    let z2 = h1.dot(w2) + b2
    let pred = sigmoid(z2)

    let dz2 = pred - y
    let dw2 = h1.transposed(withPermutations: 1, 0).dot(dz2)
    let db2 = dz2.sum(alongAxes: [0])
    let dz1 = dz2.dot(w2.transposed(withPermutations: 1, 0)) * h1 * (1 - h1)
    let dw1 = x.transposed(withPermutations: 1, 0).dot(dz1)
    let db1 = dz1.sum(alongAxes: [0])

    // Descent
    w1 -= (dw1 * learningRate)
    b1 -= (db1 * learningRate)
    w2 -= (dw2 * learningRate)
    b2 -= (db2 * learningRate)
  // }
}

// FIXME: Partitioner unreachable: Unmapped value while cloning?
#if false
@inline(never)
func testXORClassifierTraining() {
  struct MLPClassifier {
    // TODO: randomize weights once we have Tensor.random() implemented.
    var w1 = Tensor<Float>(shape: [2, 4], repeating: 0.5)
    var w2 = Tensor<Float>(shape: [4, 1], repeating: 0.5)
    var b1 = Tensor<Float>.zeros(shape: [1, 4])
    var b2 = Tensor<Float>.zeros(shape: [1, 1])

    /// - TODO: Remove when deabstraction is implemented.
    @inline(__always)
    init() {}

    @_versioned
    @inline(__always)
    func prediction(for x: Tensor<Float>) -> Tensor<Float> {
      let o1 = tanh(x ⊗ w1 + b1)
      return tanh(o1 ⊗ w2 + b2)
    }

    @_versioned
    @inline(__always)
    func prediction(for a: Bool, _ b: Bool) -> Bool {
      let x: Float = a ? 1 : 0
      let y: Float = b ? 1 : 0
      let input = Tensor([[x, y]])
      let floatPred = prediction(for: input).scalarized()
      return abs(floatPred - 1) < 0.001
    }

    @_versioned
    @inline(__always)
    func loss(of prediction: Tensor<Float>,
              from exampleOutput: Tensor<Float>) -> Float {
      return (prediction - exampleOutput).squared().mean()
    }

    @_versioned
    @inline(__always)
    mutating func train(inputBatch x: Tensor<Float>,
                        outputBatch expected: Tensor<Float>,
                        iterationCount: Int, learningRate: Float) {
      for _ in 0..<iterationCount {
        let
          mmul1 = x ⊗ w1,
          l1 = mmul1 + b1,
          o1 = sigmoid(l1),
          mmul2 = o1 ⊗ w2,
          l2 = mmul2 + b2,
          pred = sigmoid(l2)

        // Loss
        let
          sub = expected - pred,
          sqr = pow(sub, 2)

        // Gradient
        let
          dSqr = 1 / Tensor<Float>(pred.scalarCountTensor),
          dSub = 2 * sub * dSqr,
          dPred = -dSub,
          dL2 = dPred * pred * (1 - pred),
          dMmul2 = dL2,
          dB2 = dL2,
          dO1 = dMmul2 ⊗ w2.transposed(),
          dW2 = o1.transposed() ⊗ dMmul2,
          dL1 = dO1 * l1 * (1 - l1),
          dMmul1 = dL1,
          dB1 = dL1,
          dW1 = x ⊗ dMmul1

        // Descent
        w1 -= (dW1 * learningRate)
        b1 -= (dB1 * learningRate)
        w2 -= (dW2 * learningRate)
        b2 -= (dB2 * learningRate)
      }
    }
  }

  var classifier = MLPClassifier()
  classifier.train(
    inputBatch: Tensor<Float>([[0.0, 0.0], [0.0, 1.0], [1.0, 0.0], [1.0, 1.0]]),
    outputBatch: Tensor<Float>([[0.0], [1.0], [1.0], [0.0]]),
    iterationCount: 1000,
    learningRate: 0.2
  )
  // TODO: Uncomment once send/receive is fixed.
  // expectEqual(classifier.prediction(for: false, false), false)
  // expectEqual(classifier.prediction(for: false, true), true)
  // expectEqual(classifier.prediction(for: true, false), true)
  // expectEqual(classifier.prediction(for: true, true), false)
}
TensorTests.testCPUAndGPU("XORClassifierTraining", testXORClassifierTraining)
#endif

@inline(never)
func testRankGetter() {
  let x = Tensor([[1.0, 2.0, 3.0], [4.0, 5.0, 6.0]])
  expectEqual(2, x.rank)
}
TensorTests.testCPUAndGPU("RankGetter", testRankGetter)

// TODO: Merge into the previous example when we support code motion to avoid
// sends.
@inline(never)
func testRankGetter2() {
  let y: Tensor<Int> = .ones(shape: [1, 2, 2, 2, 2, 2, 1])
  expectEqual(7, y.rank)
}
TensorTests.testCPUAndGPU("RankGetter2", testRankGetter2)

@inline(never)
func testShapeGetter() {
  let x = Tensor([[1.0, 2.0, 3.0], [4.0, 5.0, 6.0]])
  expectEqual([2, 3], x.shape)
}
TensorTests.testCPUAndGPU("ShapeGetter", testShapeGetter)

@inline(never)
func testShapeGetter2() {
  let y: Tensor<Int> = .ones(shape: [1, 2, 2, 2, 2, 2, 1])
  expectEqual([1, 2, 2, 2, 2, 2, 1], y.shape)
}
TensorTests.testCPUAndGPU("ShapeGetter2", testShapeGetter2)

runAllTests()

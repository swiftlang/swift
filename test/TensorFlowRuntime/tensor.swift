// RUN: %target-run-simple-swift
// REQUIRES: executable_test
//
// Tensor API tests.

import TensorFlow
import TestUtils
import StdlibUnittest

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
  let scalar = Tensor(1.0)
  let matrix = Tensor([[1.0, 2.0, 3.0], [4.0, 5.0, 6.0]])
  expectEqual(ShapedArray(shape: [], scalars: [1]), scalar.array)
  expectEqual(ShapedArray(shape: [2, 3], scalars: [1, 2, 3, 4, 5, 6]),
              matrix.array)
}

TensorTests.testCPUAndGPU("FactoryInitializers") {
  let x = Tensor<Float>(ones: [1, 10])
  expectEqual(ShapedArray(shape: [1, 10], repeating: 1), x.array)
}

TensorTests.testCPUAndGPU("RandomInitializer") {
  let random = Tensor<Float>(
    randomNormal: [3, 4], mean: 100, stddev: 50, seed: 42
  )
  expectEqual([3, 4], random.shape)
  expectPointwiseNearlyEqual([
    137.281219, 68.1401749, 102.428467, 67.4076538, 56.9186516, 100.973923,
    107.604424, 150.683273, 195.382324, 22.3883247, 55.4706612, 118.716873
  ], random.scalars)
}

TensorTests.testCPUAndGPU("ScalarToTensorConversion") {
  let tensor = 5.makeTensor(withRank: 4)
  expectEqual([1, 1, 1, 1], tensor.shape)
  expectEqual([5], tensor.scalars)
}

TensorTests.testCPUAndGPU("DataTypeCast") {
  let x = Tensor<Int32>(ones: [5, 5])
  let ints = Tensor<Int64>(x)
  let floats = Tensor<Float>(x)
  let i8s = Tensor<Int8>(floats)
  expectEqual(ShapedArray(shape: [5, 5], repeating: 1), ints.array)
  expectEqual(ShapedArray(shape: [5, 5], repeating: 1), floats.array)
  expectEqual(ShapedArray(shape: [5, 5], repeating: 1), i8s.array)
}

TensorTests.testCPUAndGPU("BoolToNumericCast") {
  let bools = Tensor<Bool>(shape: [2, 2], scalars: [true, false, true, false])
  let ints = Tensor<Int64>(bools)
  let floats = Tensor<Float>(bools)
  let i8s = Tensor<Int8>(bools)
  expectEqual(ShapedArray(shape: [2, 2], scalars: [1, 0, 1, 0]), ints.array)
  expectEqual(ShapedArray(shape: [2, 2], scalars: [1, 0, 1, 0]), floats.array)
  expectEqual(ShapedArray(shape: [2, 2], scalars: [1, 0, 1, 0]), i8s.array)
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
  expectPointwiseNearlyEqual([0.833655, 0.833655], array.scalars,
                             byError: 0.0001)
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
  // Exercise debug logging.
  _RuntimeConfig.printsDebugLog = true

  // Shape: 1 x 4
  let x = Tensor([[1.0, 2.0, 2.0, 1.0]])
  // Shape: 4 x 2
  let w = Tensor([[1.0, 0.0], [3.0, 0.0], [2.0, 3.0], [1.0, 0.0]])
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

// FIXME: Partitioner bug (b/72997202)
#if false // Remove #if when fixed.
// This is derived from a TF Eager testcase.
TensorTests.testGPU("loopsAndConditions") {
  var a = Tensor<Int32>(6)
  var count = Tensor<Int32>(0)
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
    let x = Tensor([[x, y]])

    // FIXME: If params are declared outside of `xor`, it would crash.
    // 2 x 4
    let w1 = Tensor(
      [[-1.83586664, -0.20809225, 0.47667537, 1.90780607],
       [-1.83523219, -0.51167348, 0.15490439, 1.91018065]])
    // 1 x 4
    let b1 = Tensor(
      [[2.54353216, 0.25132703, -0.16503136, -0.85754058]])
    // 4 x 1
    let w2 = Tensor(
      [[3.04350065], [0.35590511], [-0.3252157], [3.49349223]])
    // 1 x 1
    let b2 = Tensor([[-0.74635993]])

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
                            [0.4, 0.3, 0.2, 0.1]])
    // 4 x 1
    var w2 = Tensor<Float>([[0.4],
                            [0.4],
                            [0.3],
                            [0.9]])
    var b1 = Tensor<Float>(zeros: [1, 4])
    var b2 = Tensor<Float>(zeros: [1, 1])

    /// - NOTE: This initializer must be manually declared, because the
    /// initializer logic for the variables declared above is large, and we need
    /// to mark this as inline(__always).
    /// - TODO: Remove when deabstraction is implemented.
    @inline(__always)
    init() {}

    @inline(__always)
    func prediction(for x: Tensor<Float>) -> Tensor<Float> {
      let o1 = tanh(x ⊗ w1 + b1)
      return tanh(o1 ⊗ w2 + b2)
    }
  }
  let predictFor = Tensor<Float>([[1, 0.5]])
  let classifier = MLPClassifier()
  let _ = classifier.prediction(for: predictFor)
  // TODO: Check result.
}

TensorTests.testCPUAndGPU("Reshape") {
  // 3 x 1
  let x = Tensor([[1], [2], [3]])
  let y = x.reshaped(to: [1, 3, 1, 1, 1])
  expectEqual([1, 3, 1, 1, 1], y.shape)
}

TensorTests.testCPUAndGPU("ReshapeToScalar") {
  let z = Tensor([[10]]).reshaped(to: [])
  expectEqual([], z.shape)
}

TensorTests.testCPUAndGPU("BroadcastTensor") {
  // 3 x 1
  let x = Tensor<Float>(shape: [3, 1], repeating: 0.0)
  let y = Tensor<Float>(shape: [1, 3, 1, 1, 1], repeating: 0.0)
  let result = x.broadcast(to: y)
  expectEqual([1, 3, 1, 1, 1], result.shape)
}

TensorTests.testCPU("StraightLineXORTraining") {
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
  // FIXME: Loop crasher b/73088003
  // for _ in 0..<iterationCount {
    let z1 = x.dot(w1) + b1
    let h1 = sigmoid(z1)
    let z2 = h1.dot(w2) + b2
    let pred = sigmoid(z2)

    let dz2 = pred - y
    let dw2 = h1.transposed().dot(dz2)
    let db2 = dz2.sum(alongAxes: [0])
    let dz1 = dz2.dot(w2.transposed()) * h1 * (1 - h1)
    let dw1 = x.transposed().dot(dz1)
    let db1 = dz1.sum(alongAxes: [0])

    // Descent
    w1 -= (dw1 * learningRate)
    b1 -= (db1 * learningRate)
    w2 -= (dw2 * learningRate)
    b2 -= (db2 * learningRate)

    // Update current loss
    // FIXME: Partitioner bug (b/73260623) turns this into malformed SIL.
    // Uncommend when fixed.
    //
    // loss = dz2.squared().mean()
  // }

  // Check results
  // expectLT(loss, 0.00001)
}

TensorTests.testCPU("XORClassifierTraining") {
  struct MLPClassifier {
    // TODO: randomize weights once we have Tensor(random:) is implemented.
    var w1 = Tensor<Float>(shape: [2, 4], repeating: 0.5)
    var w2 = Tensor<Float>(shape: [4, 1], repeating: 0.5)
    var b1 = Tensor<Float>(zeros: [1, 4])
    var b2 = Tensor<Float>(zeros: [1, 1])

    /// - TODO: Remove when deabstraction is implemented.
    @inline(__always)
    init() {}

    @_versioned
    @inline(__always)
    func prediction(for x: Tensor<Float>) -> Tensor<Float> {
      let o1 = sigmoid(x ⊗ w1 + b1)
      return sigmoid(o1 ⊗ w2 + b2)
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
                        outputBatch y: Tensor<Float>,
                        iterationCount: Int, learningRate: Float) {
      // FIXME: Loop crasher b/73088003
      // for _ in 0..<iterationCount {
        let z1 = x.dot(w1) + b1
        let h1 = sigmoid(z1)
        let z2 = h1.dot(w2) + b2
        let pred = sigmoid(z2)

        let dz2 = pred - y
        let dw2 = h1.transposed().dot(dz2)
        let db2 = dz2.sum(alongAxes: [0])
        let dz1 = dz2.dot(w2.transposed()) * h1 * (1 - h1)
        let dw1 = x.transposed().dot(dz1)
        let db1 = dz1.sum(alongAxes: [0])

        // Descent
        w1 -= (dw1 * learningRate)
        b1 -= (db1 * learningRate)
        w2 -= (dw2 * learningRate)
        b2 -= (db2 * learningRate)
      // }
    }
  }

  var classifier = MLPClassifier()
  classifier.train(
    inputBatch: Tensor<Float>(shape: [4, 2],
                              scalars: [0.0, 0.0,
                                        0.0, 1.0,
                                        1.0, 0.0,
                                        1.0, 1.0]),
    outputBatch: Tensor<Float>(shape: [4, 1],
                               scalars: [0.0, 1.0, 1.0, 0.0]),
    iterationCount: 1000,
    learningRate: 0.2
  )
  // TODO: Uncomment once send/receive is fixed.
  // expectEqual(classifier.prediction(for: false, false), false)
  // expectEqual(classifier.prediction(for: false, true), true)
  // expectEqual(classifier.prediction(for: true, false), true)
  // expectEqual(classifier.prediction(for: true, true), false)
}

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
  let y = Tensor<Int32>(ones: [1, 2, 2, 2, 2, 2, 1])
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
  let y = Tensor<Int32>(ones: [1, 2, 2, 2, 2, 2, 1])
  expectEqual([1, 2, 2, 2, 2, 2, 1], y.shape)
}
TensorTests.testCPUAndGPU("ShapeGetter2", testShapeGetter2)

runAllTests()

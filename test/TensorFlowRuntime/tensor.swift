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
  let scalar: Tensor<Float> = 1.0
  let matrix: Tensor<Float> = [[1.0, 2.0, 3.0], [4.0, 5.0, 6.0]]
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
  let tensor = 42.makeTensor(withRank: 4)
  expectEqual([1, 1, 1, 1], tensor.shape)
  expectEqual([42], tensor.scalars)
}

TensorTests.testCPUAndGPU("ArrayConversion") {
  let array3D = ShapedArray(shape: [2, 3, 4], repeating: 1.0)
  let tensor3D = Tensor(array3D)
  expectEqual(array3D, tensor3D.array)
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

TensorTests.test("ElementIndexing") {
  // NOTE: This tests the `subscript(index:)` method, which is distinct from
  // the `subscript(indices:)` method.
  // NOTE: cannot test multiple `Tensor.shape` or `Tensor.scalars` directly
  // until send and receive are implemented (without writing a bunch of mini
  // tests). Instead, `Tensor.array` is called to make a ShapedArray host copy
  // and the ShapedArray is tested.
  let tensor3D = Tensor<Float>(shape: [3, 4, 5],
                               scalars: Array(stride(from: 0.0, to: 60, by: 1)))
  let element2D = tensor3D[2]
  let element1D = tensor3D[1][3]
  let element0D = tensor3D[2][0][3]

  let array2D = element2D.array
  let array1D = element1D.array
  let array0D = element0D.array

  /// Test shapes
  expectEqual([4, 5], array2D.shape)
  expectEqual([5], array1D.shape)
  expectEqual([], array0D.shape)

  /// Test scalars
  expectEqual(Array(stride(from: 40.0, to: 60, by: 1)), array2D.scalars)
  expectEqual(Array(stride(from: 35.0, to: 40, by: 1)), array1D.scalars)
  expectEqual([43], array0D.scalars)
}

TensorTests.test("NestedElementIndexing") {
  // NOTE: This tests the `subscript(indices:)` method, which is distinct from
  // the `subscript(index:)` method.
  // NOTE: This test could use a clearer name, along with other "indexing"
  // tests. Note to update corresponding test names in other files
  // (ranked_tensor.test, shaped_array.test) as well.
  let tensor3D = Tensor<Float>(shape: [3, 4, 5],
                               scalars: Array(stride(from: 0.0, to: 60, by: 1)))
  let element1D = tensor3D[1, 3]
  let element0D = tensor3D[2, 0, 3]

  let array1D = element1D.array
  let array0D = element0D.array

  /// Test shapes
  expectEqual([5], array1D.shape)
  expectEqual([], array0D.shape)

  /// Test scalars
  expectEqual(Array(stride(from: 35.0, to: 40, by: 1)), array1D.scalars)
  expectEqual([43], array0D.scalars)
}

TensorTests.test("SliceIndexing") {
  // NOTE: cannot test `Tensor.shape` or `Tensor.scalars` directly until send
  // and receive are implemented (without writing a bunch of mini tests).
  // Instead, `Tensor.array` is called to make a ShapedArray host copy and the
  // ShapedArray is tested instead.
  let tensor3D = Tensor<Float>(shape: [3, 4, 5],
                               scalars: Array(stride(from: 0.0, to: 60, by: 1)))
  let slice3D = tensor3D[1..<2]
  let slice2D = tensor3D[1][0..<2]
  let slice1D = tensor3D[0][0][3..<5]

  let array3D = slice3D.array
  let array2D = slice2D.array
  let array1D = slice1D.array

  /// Test shapes
  expectEqual([1, 4, 5], array3D.shape)
  expectEqual([2, 5], array2D.shape)
  expectEqual([2], array1D.shape)

  /// Test scalars
  expectEqual(Array(stride(from: 20.0, to: 40, by: 1)), array3D.scalars)
  expectEqual(Array(stride(from: 20.0, to: 30, by: 1)), array2D.scalars)
  expectEqual(Array(stride(from: 3.0, to: 5, by: 1)), array1D.scalars)
}

TensorTests.testCPUAndGPU("Reduction") {
  // 2 x 5
  let x = Tensor<Float>([[1, 2, 3, 4, 5], [1, 2, 3, 4, 5]])
  let sum = x.sum(alongAxes: [0], keepingDimensions: true)
  expectEqual(ShapedArray(shape: [1, 5], scalars: [2, 4, 6, 8, 10]), sum.array)
}

TensorTests.testCPUAndGPU("Concatenation") {
  // 2 x 3
  let t1 = Tensor<Int32>([[0, 1, 2], [3, 4, 5]])
  // 2 x 3
  let t2 = Tensor<Int32>([[6, 7, 8], [9, 10, 11]])
  let concatenated = t1 ++ t2
  let concatenated0 = t1.concatenated(with: t2)
  let concatenated1 = t1.concatenated(with: t2, alongAxis: 1)
  expectEqual(ShapedArray(shape: [4, 3], scalars: Array(0..<12)),
              concatenated.array)
  expectEqual(ShapedArray(shape: [4, 3], scalars: Array(0..<12)),
              concatenated0.array)
  expectEqual(ShapedArray(shape: [2, 6],
                          scalars: [0, 1, 2, 6, 7, 8, 3, 4, 5, 9, 10, 11]),
              concatenated1.array)
}

TensorTests.testCPUAndGPU("ArgMax") {
  // 2 x 3
  let x = Tensor<Float>([[0, 1, 2], [3, 4, 5]])
  let argmax0 = x.argmax(alongAxis: 0)
  let argmax1 = x.argmax(alongAxis: 1)
  let scalarsArgmax = x.argmax()
  expectEqual(ShapedArray(shape: [3], scalars: [1, 1, 1]), argmax0.array)
  expectEqual(ShapedArray(shape: [2], scalars: [2, 2]), argmax1.array)
  expectEqual(5, scalarsArgmax)
}

TensorTests.testCPUAndGPU("SimpleMath") {
  let x = Tensor<Float>([1.2, 1.2])
  let y = tanh(x)
  let array = y.array
  expectEqual([2], array.shape)
  expectPointwiseNearlyEqual([0.833655, 0.833655], array.scalars,
                             byError: 0.0001)
}

TensorTests.testCPUAndGPU("Convolution") {
  let x = Tensor<Float>(shape: [1, 1, 3, 3], repeating: 0.5)
  let filter = Tensor<Float>(shape: [1, 1, 3, 3],
                             scalars: [0, 1, 0, 1, 1, 1, 0, 1, 0])
  let y = x.convolved2D(withFilter: filter, strides: [1, 1, 1, 1],
                        padding: .same)
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
  let t1 = x + y
  let t2 = t1 * t1
  let t3 = sqrt(t2)

  let array1 = t1.array
  let array2 = t2.array
  let array3 = t3.array
  expectEqual([2], array1.shape)
  expectEqual([2], array2.shape)
  expectEqual([2], array3.shape)
  expectPointwiseNearlyEqual([3.6, 3.6], array1.scalars)
  expectPointwiseNearlyEqual([12.96, 12.96], array2.scalars)
  expectPointwiseNearlyEqual([3.6, 3.6], array3.scalars)
}

TensorTests.testCPUAndGPU("XWPlusB") {
  // Shape: 1 x 4
  let x = Tensor([[1.0, 2.0, 2.0, 1.0]])
  // Shape: 4 x 2
  let w = Tensor([[1.0, 0.0], [3.0, 0.0], [2.0, 3.0], [1.0, 0.0]])
  // Shape: 2
  let b = Tensor([0.5, 0.5])
  // Shape: 1 x 2 (broadcasted)
  let result = x ⊗ w + b
  expectEqual([1, 2], result.shape)
  expectEqual([12.5, 6.5], result.scalars)
}

TensorTests.testCPUAndGPU("Transpose") {
  // 3 x 2 -> 2 x 3
  let xT = Tensor([[1, 2], [3, 4], [5, 6]]).transposed()
  let xTArray = xT.array
  expectEqual(2, xTArray.rank)
  expectEqual([2, 3], xTArray.shape)
  expectEqual([1, 3, 5, 2, 4, 6], xTArray.scalars)
}

TensorTests.testCPUAndGPU("SimpleCond") {
  func selectValue(_ pred: Bool) -> Tensor<Int32> {
  let a = Tensor<Int32>(0)
  let b = Tensor<Int32>(1)
  if pred  {
    return a
  }
    return b
  }

  expectEqual(0, selectValue(true).scalar)
}

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
  // 2 x 3 -> 1 x 3 x 1 x 2 x 1
  let matrix = Tensor<Int32>([[0, 1, 2], [3, 4, 5]])
  let reshaped = matrix.reshaped(to: [1, 3, 1, 2, 1])

  expectEqual([1, 3, 1, 2, 1], reshaped.shape)
  expectEqual(Array(0..<6), reshaped.scalars)
}

TensorTests.testCPUAndGPU("Flatten") {
  // 2 x 3 -> 6
  let matrix = Tensor<Int32>([[0, 1, 2], [3, 4, 5]])
  let flattened = matrix.flattened()

  expectEqual([6], flattened.shape)
  expectEqual(Array(0..<6), flattened.scalars)
}

TensorTests.testCPUAndGPU("Flatten0D") {
  let scalar = Tensor<Float>(5)
  let flattened = scalar.flattened()
  expectEqual([1], flattened.shape)
  expectEqual([5], flattened.scalars)
}

TensorTests.testCPUAndGPU("ReshapeToScalar") {
  // 1 x 1 -> scalar
  let z = Tensor([[10]]).reshaped(to: [])
  expectEqual([], z.shape)
}

TensorTests.testCPUAndGPU("BroadcastTensor") {
  // 2 x 3 -> 1 x 3 x 1 x 2 x 1
  let x = Tensor<Float>(shape: [2, 3], repeating: 0.0)
  let y = Tensor<Float>(shape: [1, 3, 1, 2, 1], repeating: 0.0)
  let result = x.broadcast(to: y)
  expectEqual([1, 3, 1, 2, 1], result.shape)
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

// TODO: Merge all rank/shape getter tests into one when we support code motion
// to avoid sends.

@inline(never)
func testRankGetter() {
  let tensor = Tensor<Int32>(shape: [3, 4, 5], scalars: Array(0..<60))
  expectEqual(3, tensor.rank)
}
TensorTests.testCPUAndGPU("RankGetter", testRankGetter)

@inline(never)
func testRankGetter2() {
  let vector = Tensor<Int32>([1])
  expectEqual(1, vector.rank)
}
TensorTests.testCPUAndGPU("RankGetter2", testRankGetter2)

@inline(never)
func testRankGetter3() {
  let matrix = Tensor([[1.0, 2.0, 3.0], [4.0, 5.0, 6.0]])
  expectEqual(2, matrix.rank)
}
TensorTests.testCPUAndGPU("RankGetter3", testRankGetter3)

@inline(never)
func testRankGetter4() {
  let ones = Tensor<Int32>(ones: [1, 2, 2, 2, 2, 2, 1])
  expectEqual(7, ones.rank)
}
TensorTests.testCPUAndGPU("RankGetter4", testRankGetter4)

@inline(never)
func testShapeGetter() {
  let tensor = Tensor<Int32>(shape: [3, 4, 5], scalars: Array(0..<60))
  expectEqual([3, 4, 5], tensor.shape)
}
TensorTests.testCPUAndGPU("ShapeGetter", testShapeGetter)

@inline(never)
func testShapeGetter2() {
  let vector = Tensor<Int32>([1])
  expectEqual([1], vector.shape)
}
TensorTests.testCPUAndGPU("ShapeGetter2", testShapeGetter2)

@inline(never)
func testShapeGetter3() {
  let matrix = Tensor([[1.0, 2.0, 3.0], [4.0, 5.0, 6.0]])
  expectEqual([2, 3], matrix.shape)
}
TensorTests.testCPUAndGPU("ShapeGetter3", testShapeGetter3)

@inline(never)
func testShapeGetter4() {
  let ones = Tensor<Int32>(ones: [1, 2, 2, 2, 2, 2, 1])
  expectEqual([1, 2, 2, 2, 2, 2, 1], ones.shape)
}
TensorTests.testCPUAndGPU("ShapeGetter4", testShapeGetter4)

runAllTests()

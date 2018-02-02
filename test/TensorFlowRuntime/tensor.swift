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

TensorTests.testCPUAndGPU("Initializers") {
  let x = Tensor([[1.0, 2.0, 3.0], [2.0, 4.0, 6.0]])
  expectEqual(x.units, [1.0, 2.0, 3.0, 2.0, 4.0, 6.0])
}

TensorTests.testCPUAndGPU("FactoryInitializers") {
  let x = Tensor<Float>.ones(shape: [1, 10])
  expectEqual(x.units, Array(repeating: 1, count: 10))
}

TensorTests.testCPUAndGPU("DataTypeCast") {
  let x: Tensor<Int32> = .ones(shape: [5, 5])
  let ints = Tensor<Int>(x)
  let floats = Tensor<Float>(x)
  let i8s = Tensor<Int8>(floats)
  expectEqual(ints.array, ShapedArray(shape: [5, 5], repeating: 1))
  expectEqual(floats.array, ShapedArray(shape: [5, 5], repeating: 1))
  expectEqual(i8s.array, ShapedArray(shape: [5, 5], repeating: 1))
}

TensorTests.testCPUAndGPU("Reduction") {
  let x = Tensor<Float>([[1, 2, 3, 4, 5], [1, 2, 3, 4, 5]])
  let sum = x.sum(alongAxes: 0, keepingDimensions: true)
  expectEqual(ShapedArray(shape: [1, 5], units: [2, 4, 6, 8, 10]), sum.array)
}

TensorTests.testCPUAndGPU("SimpleMath") {
  let x = Tensor<Float>([1.2, 1.2]).toDevice()
  let y = tanh(x)
  let array = y.array
  expectNearlyEqual(array.units[0], 0.833655, byError: 0.0001)
  expectNearlyEqual(array.units[1], 0.833655, byError: 0.0001)
}

#if false
TensorTests.testCPUAndGPU("Convolution") {
  let x = Tensor<Float>(shape: [1, 3, 3, 1], repeating: 0.5)
  let filter = Tensor<Float>(shape: [1, 3, 3, 1],
                             units: [0, 1, 0, 1, 1, 1, 0, 1, 0])
  // FIXME: Bug "attribute 'strides' requires a constant argument".
  let y = x.convolved2D(withFilter: filter,
                        strides: [1, 1, 1, 1], padding: .same)
}
#endif

TensorTests.testCPUAndGPU("3Adds") {
  let a = Tensor([1]).toDevice()
  let b = Tensor([2]).toDevice()
  let c = Tensor([3]).toDevice()

  let o = a + b + c
  expectEqual(o.array.units[0], 6)
}

TensorTests.testCPUAndGPU("testMultiOpMath") {
  let x = Tensor<Float>([1.2, 1.2]).toDevice()
  let y = Tensor<Float>([4.3, 4.3]).toDevice()
  let sum = x + y
  let squared = sum * sum
  let expsqr = exp(squared)
  _ = expsqr
  // TODO: Check result
}

TensorTests.testCPUAndGPU("testXWPlusB") {
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

// FIXME: The While op doesn't work on the CPU.
TensorTests.testGPU("simpleCounterLoop") {
  let maxCount = 100
  var a = Tensor<Int>(0)
  let b = Tensor<Int>(1)

  a -= b

  var count = 0
  while count < maxCount {
    a += b
    count += 1
  }
  a -= b
  expectEqual(a.scalar, 8)
}

#if false // FIXME: Exposing partitioning bugs.
@inline(never)
func testLoopsAndConditions() {
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

  expectEqual(count.scalar, 8)
}
TensorTests.testCPUAndGPU("testLoopsAndConditions", testLoopsAndConditions)
#endif

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
  expectNearlyEqual(xor(0.0, 0.0), 0.0, byError: 0.1)
  expectNearlyEqual(xor(0.0, 1.0), 1.0, byError: 0.1)
  expectNearlyEqual(xor(1.0, 0.0), 1.0, byError: 0.1)
  expectNearlyEqual(xor(1.0, 1.0), 0.0, byError: 0.1)
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
    var b1 = Tensor<Float>.zeros(shape: [1, 4]).toDevice()
    var b2 = Tensor<Float>.zeros(shape: [1, 1]).toDevice()

    @_versioned
    @_inlineable
    func prediction(for x: Tensor<Float>) -> Tensor<Float> {
      let o1 = tanh(x ⊗ w1 + b1)
      return tanh(o1 ⊗ w2 + b2)
    }
  }
  let classifier = MLPClassifier()
  let _ = classifier.prediction(for: Tensor([[1, 0.5]]).toDevice())
  // TODO: Check result.
}

TensorTests.testCPUAndGPU("Reshape") {
  let x = Tensor([[1], [2], [3]]).toDevice() // Shape 3 x 1
  let y = x.reshaped([1, 3, 1, 1, 1])
  expectEqual(y.shape, [1, 3, 1, 1, 1])
}

TensorTests.testCPUAndGPU("ReshapeScalar") {
  let z = Tensor([[10]]).toDevice().reshaped([])
  expectEqual(z.shape, [])
}

// FIXME: Partitioner gives unpredictable errors regarding send/receive.
#if false
@inline(never)
func testStraightLineXORTraining() {
  // Hyper-parameters
  let iterationCount = 1000
  let learningRate: Float = 0.2

  // Parameters
  var w1 = Tensor<Float>(shape: [2, 4], repeating: 0.5).toDevice()
  var w2 = Tensor<Float>(shape: [4, 1], repeating: 0.5).toDevice()
  var b1 = Tensor<Float>.zeros(shape: [1, 4]).toDevice()
  var b2 = Tensor<Float>.zeros(shape: [1, 1]).toDevice()

  // Training data
  let inputBatch = Tensor<Float>(
    [[0.0, 0.0], [0.0, 1.0], [1.0, 0.0], [1.0, 1.0]]
  ).toDevice()
  let outputBatch = Tensor<Float>([[0.0], [1.0], [1.0], [0.0]]).toDevice()

  // Training loop
  // FIXME: Partitioner assertion "Marking instructions out of the tensor region?"
  // for i in 0..<iterationCount {
    let mmul1 = inputBatch ⊗ w1
    let l1 = mmul1 + b1
    // FIXME: "GraphGen cannot lower a 'receive' from the host yet"
    // `sigmoid` is @_inlineable, and it should not need send/receive.
    let o1 = sigmoid(l1)
    let mmul2 = o1 ⊗ w2
    let l2 = mmul2 + b2
    // FIXME: "GraphGen cannot lower a 'receive' from the host yet"
    // `sigmoid` is @_inlineable, and it should not need send/receive.
    let pred = sigmoid(l2)

    // Loss
    let sub = outputBatch - pred
    let sqr = sub * sub
    let mean = sqr.mean()

    // Gradient
    let dSqr = 1 / Tensor<Float>(pred.unitCountTensor)
    let dSub = 2 * sub * dSqr
    let dPred = -dSub
    let dL2 = dPred * pred * (1 - pred)
    let dMmul2 = dL2
    let dB2 = dL2
    let dO1 = dMmul2 ⊗ w2.transpose
    let dW2 = o1.transpose ⊗ dMmul2
    let dL1 = dO1 * l1 * (1 - l1)
    let dMmul1 = dL1
    let dB1 = dL1
    let dW1 = inputBatch ⊗ dMmul1

    // Descent
    w1 -= (dW1 * learningRate)
    b1 -= (dB1 * learningRate)
    w2 -= (dW2 * learningRate)
    b2 -= (dB2 * learningRate)
  // }
}
TensorTests.testCPUAndGPU("StraightLineXORTraining", testStraightLineXORTraining)
#endif

// FIXME: Partitioner assertion "Marking instructions out of the tensor region?"
#if false
@inline(never)
func testXORClassifierTraining() {
  struct MLPClassifier {
    // TODO: randomize weights once we have Tensor.random() implemented.
    var w1 = Tensor<Float>(shape: [2, 4], repeating: 0.5).toDevice()
    var w2 = Tensor<Float>(shape: [4, 1], repeating: 0.5).toDevice()
    var b1 = Tensor<Float>.zeros(shape: [1, 4]).toDevice()
    var b2 = Tensor<Float>.zeros(shape: [1, 1]).toDevice()

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
      /// FIXME: Partitioner assertion: "BasicBlock not in our subset".
      for i in 0..<iterationCount {
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
          sqr = pow(sub, 2),
          mean = sqr.mean()

        // Gradient
        let
          dSqr = 1 / Tensor<Float>(pred.unitCountTensor),
          dSub = 2 * sub * dSqr,
          dPred = -dSub,
          dL2 = dPred * pred * (1 - pred),
          dMmul2 = dL2,
          dB2 = dL2,
          dO1 = dMmul2 ⊗ w2.transpose,
          dW2 = o1.transpose ⊗ dMmul2,
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
  expectEqual(x.rank, 2)
}
TensorTests.testCPUAndGPU("RankGetter", testRankGetter)

// TODO: Merge into the previous example when we support code motion to avoid
// sends.
@inline(never)
func testRankGetter2() {
  let y: Tensor<Int> = .ones(shape: [1, 2, 2, 2, 2, 2, 1])
  expectEqual(y.rank, 7)
}
TensorTests.testCPUAndGPU("RankGetter2", testRankGetter2)

@inline(never)
func testShapeGetter() {
  let x = Tensor([[1.0, 2.0, 3.0], [4.0, 5.0, 6.0]])
  expectEqual(x.shape, [2, 3])
}
TensorTests.testCPUAndGPU("ShapeGetter", testShapeGetter)

@inline(never)
func testShapeGetter2() {
  let y: Tensor<Int> = .ones(shape: [1, 2, 2, 2, 2, 2, 1])
  expectEqual(y.shape, [1, 2, 2, 2, 2, 2, 1])
}
TensorTests.testCPUAndGPU("ShapeGetter2", testShapeGetter2)

runAllTests()

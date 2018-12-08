// RUN: %target-run-simple-swift %swift-tensorflow-test-run-extra-options
// RUN: %target-run-dynamic-compilation-swift %swift-tensorflow-test-run-extra-options
// REQUIRES: executable_test
// REQUIRES: swift_test_mode_optimize
//
// Simple model tests.
//
// NOTE: Only extremely simple models, such as MLP with 2000-iteration training
// loops, should be added here so that the testing time won't slow down
// too much.

import TensorFlow
#if TPU
import TensorFlowUnittestTPU
#else
import TensorFlowUnittest
#endif
import StdlibUnittest

var ModelTests = TestSuite("Model")

ModelTests.testAllBackends("StraightLineXORTraining") {
  // FIXME: TPU execution on TAP is timing out. (b/74155319)
  guard !_RuntimeConfig.executionMode.isTPU else { return }
  // FIXME: GPU training won't converge.
#if CUDA
  return
#endif
  // FIXME: Debug and fix this test in eager mode.
  guard !_RuntimeConfig.usesTFEagerAPI else { return }

  // Hyper-parameters
  let iterationCount = 2000
  let learningRate: Float = 0.2
  var loss = Float.infinity

  // Parameters
  var rng = ARC4RandomNumberGenerator(seed: 42)
  var w1 = Tensor<Float>(randomUniform: [2, 4], generator: &rng)
  var w2 = Tensor<Float>(randomUniform: [4, 1], generator: &rng)
  var b1 = Tensor<Float>(zeros: [1, 4])
  var b2 = Tensor<Float>(zeros: [1, 1])

  // Training data
  let x: Tensor<Float> = [[0, 0], [0, 1], [1, 0], [1, 1]]
  let y: Tensor<Float> = [[0], [1], [1], [0]]

  // Training loop
  // FIXME: Use a for-loop when it can be properly deabstracted.
  var i = 0
  repeat {
    // Forward pass
    let z1 = matmul(x, w1) + b1
    let h1 = sigmoid(z1)
    let z2 = matmul(h1, w2) + b2
    let pred = sigmoid(z2)

    // Backward pass
    let dz2 = pred - y
    let dw2 = matmul(h1.transposed(withPermutations: 1, 0), dz2)
    let db2 = dz2.sum(squeezingAxes: 0)
    let dz1 = matmul(dz2, w2.transposed(withPermutations: 1, 0)) * h1 * (1 - h1)
    let dw1 = matmul(x.transposed(withPermutations: 1, 0), dz1)
    let db1 = dz1.sum(squeezingAxes: 0)

    // Gradient descent
    w1 -= dw1 * learningRate
    b1 -= db1 * learningRate
    w2 -= dw2 * learningRate
    b2 -= db2 * learningRate

    // Update current loss
    loss = dz2.squared().mean(squeezingAxes: 1, 0).scalarized()

    // Update iteration count
    i += 1
  } while i < iterationCount

  // Check results
  expectLT(loss, 0.01)
}

ModelTests.testAllBackends("XORClassifierTraining") {
  // FIXME: XORClassifierTraining_TPU crashes with SIGSEGV. (b/74155319)
  guard !_RuntimeConfig.executionMode.isTPU else { return }
  // FIXME: GPU training won't converge.
#if CUDA
  return
#endif
  // FIXME: Debug and fix this test in eager mode.
  guard !_RuntimeConfig.usesTFEagerAPI else { return }

  // The classifier struct.
  struct MLPClassifier {
    // Parameters
    var rng = ARC4RandomNumberGenerator(seed: 42)
    var w1, w2, b1, b2: Tensor<Float>

    init() {
      w1 = Tensor(randomUniform: [2, 4], generator: &rng)
      w2 = Tensor(randomUniform: [4, 1], generator: &rng)
      b1 = Tensor(zeros: [1, 4])
      b2 = Tensor(zeros: [1, 1])
    }

    func prediction(for x: Tensor<Float>) -> Tensor<Float> {
      let o1 = sigmoid(matmul(x, w1) + b1)
      return sigmoid(matmul(o1, w2) + b2)
    }

    func prediction(for x: Bool, _ y: Bool) -> Bool {
      let input = Tensor<Float>(Tensor([x, y]).reshaped(to: [1, 2]))
      let floatPred = prediction(for: input).scalarized()
      return abs(floatPred - 1) < 0.1
    }

    func loss(of prediction: Tensor<Float>,
              from exampleOutput: Tensor<Float>) -> Float {
      return (prediction - exampleOutput).squared()
        .mean(squeezingAxes: 0, 1).scalarized()
    }

    mutating func train(inputBatch x: Tensor<Float>,
                        outputBatch y: Tensor<Float>,
                        iterationCount: Int, learningRate: Float) {
      // FIXME: Loop crasher b/73088003
      var i = 0
      repeat {
        let z1 = matmul(x, w1) + b1
        let h1 = sigmoid(z1)
        let z2 = matmul(h1, w2) + b2
        let pred = sigmoid(z2)

        let dz2 = pred - y
        let dw2 = matmul(h1.transposed(withPermutations: 1, 0), dz2)
        let db2 = dz2.sum(squeezingAxes: 0)
        let dz1 = matmul(dz2, w2.transposed(withPermutations: 1, 0)) * h1 * (1 - h1)
        let dw1 = matmul(x.transposed(withPermutations: 1, 0), dz1)
        let db1 = dz1.sum(squeezingAxes: 0)

        // Gradient descent
        w1 -= dw1 * learningRate
        b1 -= db1 * learningRate
        w2 -= dw2 * learningRate
        b2 -= db2 * learningRate

        // Update iteration count
        i += 1
      } while i < iterationCount
    }
  }

  var classifier = MLPClassifier()
  classifier.train(
    inputBatch: [[0, 0], [0, 1], [1, 0], [1, 1]],
    outputBatch: [[0], [1], [1], [0]],
    iterationCount: 2000,
    learningRate: 0.2
  )
  // TODO: Add other expectations once code motion helps avoid send/receive.
  expectEqual(classifier.prediction(for: true, false), true)
}

runAllTests()

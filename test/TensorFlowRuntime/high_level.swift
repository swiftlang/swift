// RUN: %target-run-simple-swift
// REQUIRES: executable_test
//
// High level API tests.
//
// NOTE: Only extremely simple models should be added here so that the testing
// time won't slow down too much.

import TensorFlow
#if TPU
import TestUtilsTPU
#else
import TestUtils
#endif
import StdlibUnittest

var HighLevelTests = TestSuite("HighLevel")

HighLevelTests.testAllBackends("ConvolutionLayer") {
  let convLayer = Convolution2DLayer<Float>(
    filter: Tensor<Float>(shape: [1, 1, 3, 3],
                          scalars: [0, 1, 0, 1, 1, 1, 0, 1, 0]),
    strides: (1, 1, 1, 1),
    padding: .same
  )
  let input = Tensor<Float>(shape: [1, 1, 3, 3], repeating: 0.5)
  let output = convLayer.applied(to: input)
  let (_, dParameters) = convLayer.gradient(for: input,
                                            backpropagating: 1)
  expectEqual(ShapedArray(shape: [1, 1, 3, 3],
                          scalars: [0.5, 1.5, 0.5,
                                    0.5, 1.5, 0.5,
                                    0.5, 1.5, 0.5]),
              output.array)
  // FIXME: Check other gradients.
}

HighLevelTests.testAllBackends("FullyConnectedRelu") {
  let denseLayer = FullyConnectedLayer<Float>(
    // 2 x 4
    weight: Tensor<Float>([[1, -1, 1, -1],
                           [-1, 1, -1, 1]]),
    // 1 x 4
    bias: Tensor<Float>([[0.5, -0.5, -0.5, 0.5]]))
  // 1 x 2
  let input = Tensor<Float>([[-0.5, 0.5]])
  let output = relu(denseLayer.applied(to: input))
  let (dInput, dParameters) = denseLayer.gradient(for: input,
                                                  backpropagating: 1)
  let dWeight = dParameters.weight
  let dBias = dParameters.bias
  expectEqual(ShapedArray(shape: [1, 4], scalars: [0, 0.5, 0, 1.5]),
              output.array)
  expectEqual(ShapedArray(shape: [1, 2],
                          scalars: [0.0, 0.0]),
              dInput.array)
  expectEqual(ShapedArray(shape: [2, 4],
                          scalars: [-0.5, -0.5, -0.5, -0.5,
                                    0.5, 0.5, 0.5, 0.5]),
              dWeight.array)
  expectEqual(ShapedArray(shape: [1, 4], scalars: [1, 1, 1, 1]),
              dBias.array)
}

#if CPU && !CUDA
runAllTestsWithRemoteSession()
#else
runAllTests()
#endif // CPU && !CUDA

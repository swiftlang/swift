// RUN: %target-run-eager-swift
//
// REQUIRES: executable_test
//
// High-level model AD runtime tests.

import TensorFlow
import StdlibUnittest
import TensorFlowUnittest

var ModelADTests = TestSuite("ModelAD")

public protocol Layer: Differentiable & KeyPathIterable
    where AllDifferentiableVariables: KeyPathIterable {
    /// The input type of the layer.
    associatedtype Input: Differentiable
    /// The output type of the layer.
    associatedtype Output: Differentiable

    /// Returns the output obtained from applying to an input.
    @differentiable(wrt: (self, input))
    func applied(to input: Input) -> Output
}

public extension Layer {
    func valueWithPullback(at input: Input)
        -> (output: Output,
            pullback: (Output.CotangentVector)
                -> (layerGradient: CotangentVector, inputGradient: Input.CotangentVector)) {
        let (out, pullback) = _valueWithPullback(at: self, input, in: Self.applied(to:))
        return (out, pullback)
    }
}

public struct Dense<Scalar>: Layer
    where Scalar : FloatingPoint & Differentiable & TensorFlowScalar {
    public var weight: Tensor<Scalar>
    public var bias: Tensor<Scalar>

    @differentiable(wrt: (self, input))
    public func applied(to input: Tensor<Scalar>) -> Tensor<Scalar> {
        return matmul(input, weight) + bias
    }
}

public extension Dense where Scalar : BinaryFloatingPoint,
                             Scalar.RawSignificand : FixedWidthInteger {
    init(inputSize: Int, outputSize: Int) {
        self.init(weight: Tensor(randomNormal: [Int32(inputSize), Int32(outputSize)]),
                  bias: Tensor(randomNormal: [Int32(outputSize)]))
    }
}

ModelADTests.testAllBackends("DenseLayer") {
  let ones = Tensor<Float>(ones: [2, 2])
  let dense = Dense<Float>(weight: ones, bias: ones)
  let grad = gradient(at: dense) { dense in
    dense.applied(to: ones).sum()
  }
  let expected = Dense<Float>.AllDifferentiableVariables(weight: ones * 2, bias: ones)
  expectEqual(expected, grad)
}

runAllTests()

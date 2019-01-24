// RUN: %target-run-eager-swift %swift-tensorflow-test-run-extra-options
// REQUIRES: executable_test
// REQUIRES: swift_test_mode_optimize
//
// Tracer tests.

import TensorFlow
#if TPU
import TensorFlowUnittestTPU
#else
import TensorFlowUnittest
#endif
import StdlibUnittest

var TracerTests = TestSuite("Tracer")

typealias Data = Tensor<Float>

typealias Result = Tensor<Float>

extension Tensor : TensorArrayProtocolEnhanced {
  public func createInstance(_owning inputs: [CTensorHandle]) -> Tensor {
    assert(inputs.count == 1)
    return Tensor(handle: TensorHandle<Scalar>(_owning: inputs[0]))
  }
}

TracerTests.testAllBackends("Basic") {
  func tracee(state: Tensor<Float>, data: Data) -> (Tensor<Float>, Result) {
    return (state + data, data)
  }

  // TODO: if we instead write Tensor(2.0), which defaults to Tensor<Double>,
  // the resulting error message is hard to understand:
  // error: cannot convert value of type '(Tensor<Float>, Data) ->
  // (Tensor<Float>, Result)' (aka '(Tensor<Float>, Tensor<Float>) ->
  // (Tensor<Float>, Tensor<Float>)') to expected argument type '(_, _) -> (_,
  // _)'
  _RuntimeConfig.printsDebugLog = true
  let state = Tensor<Float>(2.0)
  let data = Tensor<Float>(3.0)
  let tracedFn = trace(with: state, in: tracee)
  let (newState, result) = tracedFn(state, data)
  
  _hostOp(newState)
  expectNearlyEqualWithScalarTensor(5.0, newState)

  _hostOp(result)
  expectNearlyEqualWithScalarTensor(3.0, result)
}

runAllTests()

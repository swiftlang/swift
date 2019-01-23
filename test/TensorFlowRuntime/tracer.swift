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
    let tmp = Tensor<Float>(1.0)
    return (tmp, tmp + data)
  }

  // TODO: if we instead write Tensor(2.0), which defaults to Tensor<Double>,
  // the resulting error message is hard to understand:
  // error: cannot convert value of type '(Tensor<Float>, Data) ->
  // (Tensor<Float>, Result)' (aka '(Tensor<Float>, Tensor<Float>) ->
  // (Tensor<Float>, Tensor<Float>)') to expected argument type '(_, _) -> (_,
  // _)'
  let state = Tensor<Float>(2.0)
  let data = Tensor<Float>(3.0)
  let tracedFn = trace(with: state, in: tracee)
  let (newState, result) = tracedFn(state, data)
  
  _hostOp(newState)
  expectNearlyEqualWithScalarTensor(1.0, newState)

  _hostOp(result)
  expectNearlyEqualWithScalarTensor(4.0, result)
}

TracerTests.testAllBackends("Advanced") {
  typealias Model = [Tensor<Float>]

  typealias Optimizer = [Tensor<Float>]

  struct State : TensorArrayProtocolEnhanced {
    var model: Model = [Tensor<Float>(1.0), Tensor<Float>(2.0)]
    var optimizer: Optimizer = [Tensor<Float>(1.0), Tensor<Float>(2.0)]

    public func _unpackTensorHandles(into address: UnsafeMutablePointer<CTensorHandle>?) {
      print("Calling State._unpackTensorHandles().")
      var ptr = address
      model._unpackTensorHandles(into: ptr)
      ptr = ptr!.advanced(by: Int(model._tensorHandleCount))
      optimizer._unpackTensorHandles(into: ptr)
    }
    public var _tensorHandleCount: Int32 {
      return model._tensorHandleCount + optimizer._tensorHandleCount
    }

    func createInstance(_owning inputs: [CTensorHandle]) -> State {
      assert(inputs.count == 4)
      var abstractState = State()
      abstractState.model = [Tensor(handle: TensorHandle<Float>(_owning: inputs[0])), Tensor(handle: TensorHandle<Float>(_owning: inputs[1]))]
      abstractState.optimizer = [Tensor(handle: TensorHandle<Float>(_owning: inputs[2])), Tensor(handle: TensorHandle<Float>(_owning: inputs[3]))]
      return abstractState
    }
  }

  func tracee(state: State, data: Data) -> (State, Result) {
    print("Running tracee()")
    var tmp = Tensor<Float>(0.0)
    for i in 0..<state.model.count {
      tmp += state.model[i] * state.optimizer[i]
    }

    print("Creating return value()")
    var newState = state
    newState.model[0] = state.model[0] + state.model[1]
    let ret = (newState, tmp + data)
    return ret
  }

  let state = State()
  let data = Tensor<Float>(3.0)
  let tracedFn = trace(with: state, in: tracee)
  let (newState, result) = tracedFn(state, data)

  _hostOp(newState) // should be State(model: [3.0, 2.0], optimizer: [1.0, 2.0])
  expectNearlyEqualWithScalarTensor(3.0, newState.model[0])
  expectNearlyEqualWithScalarTensor(2.0, newState.model[1])
  expectNearlyEqualWithScalarTensor(1.0, newState.optimizer[0])
  expectNearlyEqualWithScalarTensor(2.0, newState.optimizer[1])
  
  _hostOp(result) // should be 8.0
  expectNearlyEqualWithScalarTensor(8.0, result)
}

runAllTests()

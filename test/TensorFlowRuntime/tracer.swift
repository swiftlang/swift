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

extension Tensor : _TensorArrayProtocolEnhanced {
  public func _makeInstance<C: Collection>(owning inputs: C) -> Tensor
    where C.Element == CTensorHandle {
    assert(inputs.count == 1)
    return Tensor(handle: TensorHandle<Scalar>(_owning: inputs.first!))
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
  let tracedFn = _graph(with: state, in: tracee)
  let (newState, result) = tracedFn(state, data)
  
  _hostOp(newState)
  expectNearlyEqualWithScalarTensor(5.0, newState)

  _hostOp(result)
  expectNearlyEqualWithScalarTensor(3.0, result)

  // A second call to `tracedFn` with different input data.
  let data2 = Tensor<Float>(1.0)
  let (newState2, result2) = tracedFn(newState, data2)

  _hostOp(newState2)
  expectNearlyEqualWithScalarTensor(6.0, newState2)

  _hostOp(result2)
  expectNearlyEqualWithScalarTensor(1.0, result2)
}

TracerTests.testAllBackends("AllDtypeSupport") {
  func tracee<S: TensorGroup>(state: S, data: S) -> (S, S) {
    return (state, data)
  }

  func traceAndRun<T>(_ v: T) -> (Tensor<T>, Tensor<T>)
    where T: TensorFlowScalar & Numeric {
    let state = Tensor<T>(v)
    let data = Tensor<T>(v + 4)
    let tracedFn = _graph(with: state, in: tracee)
    return tracedFn(state, data)
  }

  func traceAndCheckNearlyEqual<T>(_ v: T)
    where T: TensorFlowScalar & FloatingPoint & ExpressibleByFloatLiteral {
    let (newState, newData) = traceAndRun(v)
    expectNearlyEqualWithScalarTensor(v, newState)
    expectNearlyEqualWithScalarTensor(v + 4, newData)
  }

  func traceAndCheckEqual<T>(_ v: T)
    where T: TensorFlowScalar & Numeric & Comparable {
    let (newState, newData) = traceAndRun(v)
    expectEqualWithScalarTensor(v, newState)
    expectEqualWithScalarTensor(v + 4, newData)
  }

  // FloatingPoint
  let f: Float = 1.0
  traceAndCheckNearlyEqual(f)
  let d: Double = 2.0
  traceAndCheckNearlyEqual(d)

  // Int variants
  let ui8: UInt8 = 1
  traceAndCheckEqual(ui8)
  let i8: Int8 = 2
  traceAndCheckEqual(i8)
  let ui16: UInt16 = 3
  traceAndCheckEqual(ui16)
  let i16: Int16 = 4
  traceAndCheckEqual(i16)
  let ui32: UInt32 = 5
  traceAndCheckEqual(ui32)
  let i32: Int32 = 6
  traceAndCheckEqual(i32)
  let ui64: UInt64 = 7
  traceAndCheckEqual(ui64)
  let i64: Int64 = 8
  traceAndCheckEqual(i64)
}

TracerTests.testAllBackends("Basic_IntermediateTensors") {
  func tracee(state: Tensor<Float>, data: Data) -> (Tensor<Float>, Result) {
    // Create an intermediate tensor value, which the tracing infra needs to
    // convert into a placeholder input into the generated trace graph function.
    let tmp = Tensor<Float>(1.0)
    return (tmp, tmp + data)
  }

  let state = Tensor<Float>(2.0)
  let data = Tensor<Float>(3.0)
  let tracedFn = _graph(with: state, in: tracee)
  let (newState, result) = tracedFn(state, data)

  _hostOp(newState)
  expectNearlyEqualWithScalarTensor(1.0, newState)

  _hostOp(result)
  expectNearlyEqualWithScalarTensor(4.0, result)
}

TracerTests.testAllBackends("Advanced") {
  typealias Model = [Tensor<Float>]

  typealias Optimizer = [Tensor<Float>]

  struct State : _TensorArrayProtocolEnhanced {
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

    func _makeInstance<C: Collection>(owning inputs: C) -> State
      where C.Element == CTensorHandle {
      assert(inputs.count == 4)
      var abstractState = State()
      let index0 = inputs.startIndex
      let index1 = inputs.index(after: index0)
      abstractState.model = [Tensor(handle: TensorHandle<Float>(_owning: inputs[index0])),
                             Tensor(handle: TensorHandle<Float>(_owning: inputs[index1]))]
      let index2 = inputs.index(after: index1)
      let index3 = inputs.index(after: index2)
      abstractState.optimizer = [Tensor(handle: TensorHandle<Float>(_owning: inputs[index2])),
                                 Tensor(handle: TensorHandle<Float>(_owning: inputs[index3]))]
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
  let tracedFn = _graph(with: state, in: tracee)
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

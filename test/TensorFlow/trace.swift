import CTensorFlow
import TensorFlow

public func test1() {
  let x = Tensor<Float>(1.0)
  _ = x + x
}

/////////////////////////
// Test 2: trace through func calls with input and return values
////////////////////////
@inline(never)
func test2Helper(_ x: Tensor<Float>, _ y: Tensor<Float>) -> Tensor<Float> {
  return x + y
}

public func test2() {
  let x = Tensor<Float>(1.0)
  let y = Tensor<Float>(2.0)
  _ = test2Helper(x, y)
}

/////////////////////////
// Test 3: conditional control flow 
////////////////////////
let b = false

@inline(never)
func readBool() -> Bool {
  return b
}

// These helpers are to help control the tensor program regions.
@inline(never)
func test3HelperX() -> Tensor<Float> {
  return Tensor<Float>(1.0)
}

@inline(never)
func test3HelperAdd(_ x: Tensor<Float>) -> Tensor<Float> {
  return x + x
}

@inline(never)
func test3HelperSub(_ x: Tensor<Float>) -> Tensor<Float> {
  return x - x
}

public func test3() {
  let x = test3HelperX()
  if readBool() {
    _ = test3HelperAdd(x)
  } else {
    _ = test3HelperSub(x)
  }
}

/////////////////////////
// Test 4: parameterize # layers
////////////////////////

let intGlobal = 3

@inline(never)
func readInt() -> Int {
  return intGlobal
}

@inline(never)
func test4HelperLayer(_ x: Tensor<Float>) -> Tensor<Float> {
  return x + x
}

public func test4() {
  var x = test3HelperX()
  let layerCount = readInt()
  for _ in 0..<layerCount {
    x = test4HelperLayer(x)
  }
}

/////////////////////////
// Test 6: trace with input and return values
////////////////////////
@inline(never)
func dataset() -> Tensor<Float> {
  return Tensor<Float>(1.0)
}

@inline(never)
func model(_ x: Tensor<Float>) -> Tensor<Float> {
  return x + x
}

@inline(never)
func model2(_ x: Tensor<Float>, _ y: Tensor<Float>) -> Tensor<Float> {
  return x + y
}


/////////////////////////
// Test 7: use the abstract CTensorHandle as the "symbol table" to track
// intermediate and result tensors in the trace region.
////////////////////////

// @inline(never)
// func test7() -> Tensor<Float> {
//   let x = dataset()
//   let y = model(x)
//   return model2(x, y)
// }

// Should return 1.0, not 2.0.
@inline(never)
func test7() -> Tensor<Float> {
  let x = dataset()
  _ = model(x)
  return x
}

/////////////////////////
// Test 8: Take and return a struct conforming to TensorGroup
////////////////////////

// extension Tensor : TensorModel {
// }

// extension TensorPair : TensorModel {
// }

// TODO: try other dtype
typealias Model = TensorPair<Tensor<Float>, Tensor<Float>>

// This crashes GPE
// @inline(never)
// func createTensorPair() -> Model {
//   return TensorPair(Tensor<Float>(1.0), Tensor<Float>(2.0))
// }


@inline(never)
func createScalarTensor(_ f: Float) -> Tensor<Float> {
  return Tensor<Float>(f)
}

@inline(never)
func test8(_ m: Model) -> Model {
  let new_x = model(m.first)
  let new_y = test2Helper(m.first, m.second)
  return Model(new_x, new_y)
}

/////////////////////////
// Test 9 and 10: Do not use functional API; takes model and optimizer as inout
// via TensorArrayProtocol, and also handles input data and result tensors
// (e.g. can be metrics).
////////////////////////

typealias Data = Tensor<Float>

typealias Result = Tensor<Float>

extension Tensor : TensorArrayProtocolEnhanced {
  public func createInstance(_owning inputs: [CTensorHandle]) -> Tensor {
    assert(inputs.count == 1)
    return Tensor(handle: TensorHandle<Scalar>(_owning: inputs[0]))
  }
}

func test9(state: Tensor<Float>, data: Data) -> (Tensor<Float>, Result) {
  let tmp = Tensor<Float>(1.0)
  // for i in 0..<state.model.count {
  //   tmp += state.model[i] * state.optimizer[i]
  // }
  // TODO: here we mutate state.model and optimizer
  // To return some original part of state, need to extend code.
  // return (state, tmp + data)
  
  // return (State(), tmp + data)
  return (tmp, tmp + data)
}

typealias Model2 = [Tensor<Float>]

typealias Optimizer = [Tensor<Float>]

struct State : TensorArrayProtocolEnhanced {
  var model: Model2 = [Tensor<Float>(1.0), Tensor<Float>(2.0)] // Model2()
  var optimizer: Optimizer = [Tensor<Float>(1.0), Tensor<Float>(2.0)] // Optimizer()

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

@inline(never)
func test10(state: State, data: Data) -> (State, Result) {
  print("Running test10()")
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



public func driver() {
  _RuntimeConfig.printsDebugLog = false

  // let tracedFn = trace(test1)
  // let tracedFn = trace(test2)
  // let tracedFn = trace(test3)
  // let tracedFn = trace(test4)
  // tracedFn()

  // test 5: with device
  // let tracedFn = trace(test1)
  // withDevice(.gpu) {
  //   tracedFn()
  // }

  // test 6
  // let datasetTracedFn = trace(dataset)
  // let x: Tensor<Float> = withDevice(.cpu) {
  //   return datasetTracedFn()
  // }
  // _hostOp(x)

  // let modelTracedFn = trace(model)
  // let y: Tensor<Float> = withDevice(.gpu) {
  //   return modelTracedFn(x)
  // }
  // _hostOp(y)  // Should be 2.0
  // // let x = Tensor<Float>(1.0)
  // // let y = Tensor<Float>(2.0)
  // // let z = model2(x,y)
  // let model2TracedFn = trace(model2)
  // let z: Tensor<Float> = withDevice(.gpu) {
  //   return model2TracedFn(x, y)
  // }
  // _hostOp(z)  // Should be 3.0

  // let tracedFn = trace(test7)
  // let z = tracedFn()
  // _hostOp(z)

  // test 8
  // let m = TensorPair(createScalarTensor(1.0), createScalarTensor(2.0))
  // _hostOp(m)

  // let tracedFn = trace(test8)
  // // TODO: cannot create m above here, because before traceFn() is called, we
  // // are still in tracing mode according to the current impl.
  // let newM = tracedFn(m)
  // _hostOp(newM) // should be (2.0, 3.0)

  // test 9
  let state = createScalarTensor(2.0)
  let data = createScalarTensor(3.0) // Data(3.0)
  let tracedFn = trace(with: state, in: test9)
  let (newState, result) = tracedFn(state, data)
  _hostOp(newState) // should be 1.0
  _hostOp(result) // should be 4.0

  // test 10
  // let state = State()
  // let data = createScalarTensor(3.0) // Data(3.0)
  // let tracedFn = trace(with: state, in: test10)
  // let (newState, result) = tracedFn(state, data)
  // _hostOp(newState) // should be State(model: [3.0, 2.0], optimizer: [1.0, 2.0])
  // _hostOp(result) // should be 8.0
}

driver()

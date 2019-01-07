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

public func driver() {
  _RuntimeConfig.printsDebugLog = true
  // let tracedFn = trace(test1)
  // let tracedFn = trace(test2)
  // let tracedFn = trace(test3)
  // let tracedFn = trace(test4)

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
  // _hostOp(y)

  // let model2TracedFn = trace(model2)
  // let z: Tensor<Float> = withDevice(.gpu) {
  //   return model2TracedFn(x, y)
  // }
  // _hostOp(z)

  let tracedFn = trace(test7)
  let z = tracedFn()
  _hostOp(z)
}

driver()

import CTensorFlow
import TensorFlow

// public func test1() {
//   let x = Tensor<Float>(1.0)
//   _ = x + x
// }

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
  for i in 0..<layerCount {
    x = test4HelperLayer(x)
  }
}


public func driver() {
  _RuntimeConfig.printsDebugLog = true
  // let tracedFn = trace(test1)
  // let tracedFn = trace(test2)
  // let tracedFn = trace(test3)
  let tracedFn = trace(test4)
  tracedFn()
}

driver()

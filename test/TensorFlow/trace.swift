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

// let b = true

// @inline(never)
// func readBool() -> Bool {
//   return b
// }

// public func test2() {
//   let x = Tensor<Float>(1.0)
//   if readBool() {
//     _ = x + x
//   } else {
//     _ = x - x
//   }
// }

public func driver() {
  _RuntimeConfig.printsDebugLog = true
  // let tracedFn = trace(test1)
  let tracedFn = trace(test2)
  tracedFn()
}

driver()

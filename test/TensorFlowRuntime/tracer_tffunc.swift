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

var TracerTests = TestSuite("TracerTFFunction")

extension Tensor : _TensorArrayProtocolEnhanced {
  public func _makeInstance<C: Collection>(owning inputs: C) -> Tensor
    where C.Element == CTensorHandle {
    assert(inputs.count == 1)
    return Tensor(handle: TensorHandle<Scalar>(_owning: inputs.first!))
  }
}

TracerTests.testAllBackends("SimpleTFFunction") {
  _RuntimeConfig.printsDebugLog = true

  func cond(i: Tensor<Int32>, n: Tensor<Int32>) -> (Tensor<Int32>, Tensor<Int32>?) {
    return (Tensor<Int32>(i .< n), nil)
  }

  @TensorFlowGraph
  func body(i: Tensor<Int32>) -> Tensor<Int32> {
    return i + 1
  }
  let zero = Tensor<Int32>(0)
  let builder = _TFFuncSpecializer(with: zero, in: cond)
  // let b = _graph(with: zero, in: cond)
  let ten = Tensor<Int32>(10)
  // print("\(b(zero, ten))")
  print("Done building.")
  let lessThan10 = builder(Tensor<Int32>(330))
  print("Less than 10 is \(lessThan10)")
  let result1:Tensor<Int32> = #tfop(
    "StatelessWhile",
    zero,
    T$dtype: [Int32.tensorFlowDataType],
    cond$func: lessThan10,
    body: body)
  print("Result is \(result1)")
}

runAllTests()

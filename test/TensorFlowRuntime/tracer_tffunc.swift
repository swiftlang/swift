// RUN: %target-run-simple-swift %swift-tensorflow-test-run-extra-options
// REQUIRES: executable_test
//
// Tracer tests.

import TensorFlow
import TensorFlowUnittest
import StdlibUnittest

var TracerTests = TestSuite("TracerTFFunction")

extension Tensor : _TensorArrayProtocolEnhanced {
  public func _makeInstance<C: Collection>(owning inputs: C) -> Tensor
    where C.Element == CTensorHandle {
    assert(inputs.count == 1)
    return Tensor(handle: TensorHandle<Scalar>(_owning: inputs.first!))
  }
}

struct State : TensorGroup {
  let i: Tensor<Int32>
  let n: Tensor<Int32>
}

TracerTests.testAllBackends("SimpleTFFunction") {
  func cond(s: State) -> Tensor<Int32> {
    return Tensor<Int32>(s.i .< s.n)
  }

  func body(s: State) -> State {
    return State(i: s.i + 1, n: s.n)
  }

  func runWhile(_ n: Int32) -> Tensor<Int32> {
    return Raw.while_(
      State(i: Tensor<Int32>(0), n: Tensor<Int32>(n)),
      cond: cond,
      body: body,
      outputShapes: [nil]).i
  }

  expectEqualWithScalarTensor(10, runWhile(10))
  expectEqualWithScalarTensor(300, runWhile(300))
}

runAllTests()

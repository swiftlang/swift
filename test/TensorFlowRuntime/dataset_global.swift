// TODO: Revert to %target-run-simple-swift once we complete send/recv support for resource/variant tensors.
// RUN: %target-run-send-recv-handle-swift
// REQUIRES: executable_test
// REQUIRES: swift_test_mode_optimize

// This file contains testing over a dataset as a global variable. This requires
// sends/recvs support for variant handles.

import TensorFlow
import TensorFlowUnittest
import StdlibUnittest

var DatasetGlobalTests = TestSuite("DatasetGlobal")

let scalars = Tensor<Float>(rangeFrom: 0, to: 5, stride: 1)
let dataset = Dataset(elements: scalars)

// This stmt makes sure the store inst into the global var `dataset` does not
// make dataset a return tensor.
//
// It can be removed when we convert return tensors to TF->Swift tensor
// transfers.
_ = Tensor<Float>(0.0)

DatasetGlobalTests.testCPUOrGPU("DataSetAsGlobalVar") {
  // This stmt makes sure the load inst from the global var `dataset` does not
  // make dataset an input arg tensor.
  //
  // It can be removed when we convert arg tensors to Swift->TF tensor
  // transfers.
  _ = scalars + scalars

  var expectedVal: Float = 0.0
  for item in dataset {
    _hostOp(item)
    expectNearlyEqualWithScalarTensor(expectedVal, item)
    expectedVal += 1.0
  }
}

runAllTests()

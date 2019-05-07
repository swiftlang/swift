// SWIFT_ENABLE_TENSORFLOW

import TensorFlow

// expected-note @+1 2 {{type declared here}}
struct OtherFileNonconforming : TensorArrayProtocol {
  var x: Tensor<Int32>
  var y: Tensor<Float>
}

// expected-note @+1 2 {{type declared here}}
struct GenericOtherFileNonconforming<T : TensorFlowScalar> : TensorArrayProtocol, Equatable {
  var x: Tensor<Int32>
  var y: Tensor<Float>
  var z: Tensor<T>
}

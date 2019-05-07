// SWIFT_ENABLE_TENSORFLOW

import TensorFlow

// expected-note @+1 4 {{type declared here}}
struct OtherFileNonconforming {
  var x: Tensor<Int32>
  var y: Tensor<Float>
}

// expected-note @+1 4 {{type declared here}}
struct GenericOtherFileNonconforming<T : TensorFlowScalar> {
  var x: Tensor<Int32>
  var y: Tensor<Float>
  var z: Tensor<T>
}

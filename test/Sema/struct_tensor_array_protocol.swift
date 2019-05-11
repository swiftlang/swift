// SWIFT_ENABLE_TENSORFLOW
// RUN: %target-swift-frontend -typecheck -verify -primary-file %s %S/Inputs/struct_tensor_array_protocol_other_module.swift

import TensorFlow

struct Empty : TensorArrayProtocol {}

struct Simple : TensorArrayProtocol, Equatable {
  var w, b: Tensor<Float>
}

struct Mixed : TensorArrayProtocol, Equatable {
  // Mutable.
  var float: Tensor<Float>
  // Immutable.
  let int: Tensor<Int32>
}

struct Generic<T: TensorGroup & Equatable, U: TensorGroup & Equatable> : TensorArrayProtocol, Equatable {
  var t: T
  var u: U
}

// Test derived conformances in disallowed contexts.

// expected-error @+1 4 {{implementation of 'TensorArrayProtocol' cannot be automatically synthesized in an extension in a different file to the type}}
extension OtherFileNonconforming : TensorArrayProtocol {}

// expected-error @+1 4 {{implementation of 'TensorArrayProtocol' cannot be automatically synthesized in an extension in a different file to the type}}
extension GenericOtherFileNonconforming : TensorArrayProtocol {}

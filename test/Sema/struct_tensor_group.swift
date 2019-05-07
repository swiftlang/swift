// SWIFT_ENABLE_TENSORFLOW
// RUN: %target-swift-frontend -typecheck -verify -primary-file %s %S/Inputs/struct_tensor_group_other_module.swift

import TensorFlow

struct Empty : TensorGroup {}

struct Simple : TensorGroup, Equatable {
  var w, b: Tensor<Float>
}

struct Mixed : TensorGroup, Equatable {
  // Mutable.
  var float: Tensor<Float>
  // Immutable.
  let int: Tensor<Int32>
}

struct Nested : TensorGroup, Equatable {
  // Immutable.
  let simple: Simple
  // Mutable.
  var mixed: Mixed
}

struct Generic<T: TensorGroup & Equatable, U: TensorGroup & Equatable> : TensorGroup, Equatable {
  var t: T
  var u: U
}

// Test derived conformances in disallowed contexts.

// expected-error @+1 2 {{implementation of 'TensorGroup' cannot be automatically synthesized in an extension in a different file to the type}}
extension OtherFileNonconforming : TensorGroup {}

// expected-error @+1 2 {{implementation of 'TensorGroup' cannot be automatically synthesized in an extension in a different file to the type}}
extension GenericOtherFileNonconforming : TensorGroup {}

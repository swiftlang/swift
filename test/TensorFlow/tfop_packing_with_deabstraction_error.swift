// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -Xllvm -tf-dump-intermediates -O -emit-sil -enable-resilience -I %t -verify %s

import TensorFlow

public func packResultsToAggregate_toGeneric<T>() -> T {
  // expected-error @+1 {{#tfop result must conform to TensorGroup or be a tuple of types that conform to TensorGroup}}
  return #tfop("SomeOp")
}

public struct GenericStruct<T> {
  let t: T
}

public func packResultsToAggregate_toGenericStruct<T>() -> GenericStruct<T> {
  // expected-error @+1 {{#tfop result must conform to TensorGroup or be a tuple of types that conform to TensorGroup}}
  return #tfop("SomeOp")
}

public struct StructWithNonTF {
  let x: Int
}

public func packResultsToAggregate_toStructWithNonTF() -> StructWithNonTF {
  // expected-error @+1 {{#tfop result must conform to TensorGroup or be a tuple of types that conform to TensorGroup}}
  return #tfop("SomeOp")
}

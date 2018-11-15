// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %S/Inputs/ExternalStructs.swift -enable-resilience -emit-module -emit-module-path %t/ExternalStructs.swiftmodule
// RUN: %target-swift-frontend -Xllvm -tf-dump-intermediates -O -emit-sil -enable-resilience -I %t -verify %s

import TensorFlow
import ExternalStructs

public func packResultsToAggregate_toGeneric<T>() -> T {
  // expected-error @+1 {{cannot extract TensorFlow result into type 'T', because 'T' contains unbound generic parameters}}
  return #tfop("SomeOp")
}

public struct GenericStruct<T> {
  let t: T
}

public func packResultsToAggregate_toGenericStruct<T>() -> GenericStruct<T> {
  // expected-error @+1 {{cannot extract TensorFlow result into type 'GenericStruct<T>', because 'GenericStruct<T>' contains unbound generic parameters}}
  return #tfop("SomeOp")
}

public class Foo {}

public func packResultsToAggregate_toClass() -> Foo {
  // expected-error @+1 {{cannot extract TensorFlow result into type 'Foo', because 'Foo' is not a TensorFlow value type or an aggregate of TensorFlow value types}}
  return #tfop("SomeOp")
}

public struct StructWithWeakReference {
  weak var foo: Foo?
}

// The weak reference causes StructWithWeakReference to be an address-only
// type, so this exercises the case where the result is an address-only type
// for a reason other than an unbound generic parameter.
public func packResultsToAggregate_toStructWithWeakReference() -> StructWithWeakReference {
  // expected-error @+1 {{cannot extract TensorFlow result into type 'StructWithWeakReference', because 'StructWithWeakReference' is not a TensorFlow value type or an aggregate of TensorFlow value types}}
  return #tfop("SomeOp")
}

public struct StructWithNonTF {
  let x: Int
}

public func packResultsToAggregate_toStructWithNonTF() -> StructWithNonTF {
  // expected-error @+1 {{cannot extract TensorFlow result into type 'StructWithNonTF', because 'StructWithNonTF' is not a TensorFlow value type or an aggregate of TensorFlow value types}}
  return #tfop("SomeOp")
}

public func packResultsToAggregate_toExternalStructNotFixedLayout() -> ExternalStructNotFixedLayout {
  // expected-error @+1 {{cannot extract TensorFlow result into type 'ExternalStructNotFixedLayout', because 'ExternalStructNotFixedLayout' is not a TensorFlow value type or an aggregate of TensorFlow value types}}
  return #tfop("SomeOp")
}

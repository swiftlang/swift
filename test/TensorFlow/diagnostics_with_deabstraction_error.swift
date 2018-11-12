// RUN: %target-swift-frontend -Xllvm -tf-dump-intermediates -O -emit-sil -verify %s

// This file contains tests that used to be in ./diagnostics.swift that produced
// expected errors in the deabstraction pass, which prevented partitioning from
// running.
//
// Once we move partitioning into the mandatory pipeline, we should be able to
// move these tests back into ./diagnostics.swift.

import TensorFlow

// Verify we reject multiple attempts to configure hardware.
public func testDeviceInvalid() {
  TensorFlow.enableTPU() // expected-note {{previous configuration is specified here}}
  TensorFlow.enableTPU() // expected-error {{device configuration specified multiple times}}
}

public func shapeError() {
  // expected-error @+1 {{tensor literal should have 9 scalars for this shape, but has 8}}
  let _ = Tensor<Float>(shape: [1, 3, 3, 1],
                        scalars: [0, 1, 0, 1, 1, 1, 0, 1])
}

public func resultPacking() {
  struct Foo { var x: Tensor<Float>, y: Float }
  // expected-error @+1 {{cannot extract TensorFlow result into type 'Foo', because 'Foo' is not a TensorFlow value type or an aggregate of TensorFlow value types}}
  let _: Foo = #tfop("SomeOp")
}

public enum X {
  case A, B
}

public func invalidAttributeArg() -> TensorHandle<Int32> {
  // expected-error@+1 {{attribute 'someAttr' cannot be an enum, struct, or tuple}}
  return #tfop("bar", someAttr: X.A)
}

public func noTensorShape() -> Tensor<Float> {
  // expected-error @+1 {{attribute 'value' must be followed by a shape attribute}}
  return Tensor(handle: #tfop("Const", dtype$dtype: Float.tensorFlowDataType, value$tensor: [17.0 as Float, 18.0]))
}

public func badTensorShape() -> Tensor<Float> {
  let badShape : TensorShape = [1]
  // expected-error @+1 {{attribute 'value' does not match the shape attribute in the number of scalar elements}}
  return Tensor(handle: #tfop("Const", dtype$dtype: Float.tensorFlowDataType, value$tensor: [17.0 as Float, 18.0], shape$shape: badShape))
}

public func metatypeAttrs() {
  // expected-error @+1 {{attribute 'T' cannot be a metatype}}
  #tfop("DummyOp", T: Float.self) as Void

  // expected-error @+1 {{attribute 'T' cannot be an array of metatypes}}
  #tfop("DummyOp", T: [Float.self]) as Void

  // expected-error @+1 {{attribute 'T' requires an integer or array of integers}}
  #tfop("DummyOp", T$dtype: Float.self) as Void

  // expected-error @+1 {{attribute 'T' requires an integer or array of integers}}
  #tfop("DummyOp", T$dtype: [Float.self]) as Void
}

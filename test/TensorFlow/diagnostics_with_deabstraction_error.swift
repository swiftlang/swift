// RUN: %target-swift-frontend -Xllvm -tf-dump-intermediates -O -emit-sil -verify %s

// This file contains tests that used to be in ./diagnostics.swift that produced
// expected errors in the deabstraction pass, which prevented partitioning from
// running.
//
// Once we move partitioning into the mandatory pipeline, we should be able to
// move these tests back into ./diagnostics.swift.

import TensorFlow

// This shows passing a non-constant value into an attribute.
// TODO: Improve the diagnostic to display the Swift parameter name instead of
// the internal TensorFlow attribute name. (In this example, it's hard to tell
// because both Swift/TensorFlow use the same name "padding".)
public func nonConstantAttribute(x: Tensor<Float>, padding: Padding) {
  // expected-error @+1 {{attribute 'padding' requires a constant argument}}
  _hostOp(x.convolved2D(withFilter: Tensor<Float>(ones: [1, 3, 3, 1]),
                        strides: (1, 1, 1, 1),
                        padding: padding))
}

public enum X {
  case A, B
}

public func invalidAttributeArg() -> TensorHandle<Int32> {
  // expected-error@+1 {{attribute 'someAttr' cannot be an enum, struct, or tuple}}
  return #tfop("bar", someAttr: X.A)
}

public func invalidAttrTensor(a: Tensor<Float>) {
   // expected-error @+1 {{attribute 'someAttr' requires a constant argument}}
   () = #tfop("foo", someAttr: a)
}

public func noTensorShape() -> Tensor<Float> {
  // expected-error @+1 {{attribute 'value' must be followed by a shape attribute}}
  return Tensor(handle: #tfop("Const", dtype: Float.self, value$tensor: [17.0 as Float, 18.0]))
}

public func badTensorShape() -> Tensor<Float> {
  let badShape : TensorShape = [1]
  // expected-error @+1 {{attribute 'value' does not match the shape attribute in the number of scalar elements}}
  return Tensor(handle: #tfop("Const", dtype: Float.self, value$tensor: [17.0 as Float, 18.0], shape: badShape))
}

// RUN: %target-swift-frontend -Xllvm -tf-dump-intermediates -O -emit-sil -verify %s

import TensorFlow

public enum X {
  case A, B
}

public func invalidAttributeArg() -> TensorHandle<Int32> {
  // expected-error@+1 {{attribute requires Bool, Int64, Double, Float, String, array thereof, [TensorShape?], or Function, but got type 'X'}}
  return #tfop("bar", someAttr: X.A)
}

public func invalidAttrTensor(a: Tensor<Float>) {
  // expected-error@+1 {{attribute requires Bool, Int64, Double, Float, String, array thereof, [TensorShape?], or Function, but got type 'Tensor<Float>'}}
   () = #tfop("foo", someAttr: a)
}

public func metatypeAttrs() {
  // expected-error@+1 {{attribute requires Bool, Int64, Double, Float, String, array thereof, [TensorShape?], or Function, but got type 'Float.Type'}}
  #tfop("DummyOp", T: Float.self) as Void

  // expected-error@+1 {{attribute requires Bool, Int64, Double, Float, String, array thereof, [TensorShape?], or Function, but got type '[Float.Type]'}}
  #tfop("DummyOp", T: [Float.self]) as Void

  // expected-error @+1 {{$dtype attribute requires TensorDataType or [TensorDataType], but got type 'Float.Type'}}
  #tfop("DummyOp", T$dtype: Float.self) as Void

  // expected-error @+1 {{$dtype attribute requires TensorDataType or [TensorDataType], but got type '[Float.Type]'}}
  #tfop("DummyOp", T$dtype: [Float.self]) as Void
}

public extension Tensor {
  // This is a theoretical operation that takes a generic scalar value as an
  // attribute.
  @inlinable @inline(__always)
  func genericAttr<T : TensorFlowScalar>(axis: T) -> Tensor {
    // expected-error@+1 {{attribute requires Bool, Int64, Double, Float, String, array thereof, [TensorShape?], or Function, but got type 'T'}}
    let ret: TensorHandle<Scalar> = #tfop("ExampleOp", handle, axis: axis, axisType$dtype: T.tensorFlowDataType)
    return Tensor<Scalar>(handle: ret)
  }
}

public func testGenericThing() {
  let a = Tensor<Float>(zeros: [1,2])
  let b = a.genericAttr(axis: 42)
  _ = b+b
}

// RUN: %target-swift-frontend -Xllvm -tf-dump-intermediates -O -emit-sil -verify %s

import TensorFlow


// Show inference of the tensor element type based on context.  This also
// exposed a SILGen bug handling cleanup generation when emitting into let
// declarations.
@_transparent
func testInferredElementResult() -> TensorHandle<Int32> {
  // expected-warning @+1 {{immutable value 'x' was never used}}
  let x : TensorHandle<Int32> = #tfop("foo")

  _ = #tfop("bar") as TensorHandle<Int32>
}

// This shows passing a non-constant value into an attribute.
// TODO: Improve the diagnostic to display the Swift parameter name instead of
// the internal TensorFlow attribute name. (In this example, it's hard to tell
// because both Swift/TensorFlow use the same name "padding".)
public func nonConstantAttribute(x: Tensor<Float>, padding: Padding) {
  // expected-error @+1 {{attribute 'padding' requires a constant argument}}
  print(x.convolved2D(withFilter: Tensor<Float>(ones: [1, 3, 3, 1]),
                      strides: (1, 1, 1, 1),
                      padding: padding))
}

public func shapeError() {
  // expected-error @+1 {{tensor literal should have 9 scalars for this shape, but has 8}}
  let _ = Tensor<Float>(shape: [1, 3, 3, 1],
                        scalars: [0, 1, 0, 1, 1, 1, 0, 1])
}


class ClassTest {
  // expected-error @+1 {{GraphGen cannot lower a 'send' to the host yet}}
  var w = Tensor<Float>(zeros: [1, 2])  // expected-warning {{value implicitly copied to the host}}
  let b = Tensor<Float>(zeros: [1, 2])

  // expected-error @+1 {{GraphGen cannot lower a 'receive}}
  var c : Tensor<Float> { return w } // expected-warning {{properties in classes always cause a copy to the accelerator}}

  func infer(input: Tensor<Float>) -> Tensor<Float> {
    return input
  }
}

public func f() {
  let x = ClassTest()
  let y = x.infer(input: Tensor<Float>(ones: [2, 1]))
  _ = y+y
  // expected-note @+1 {{value used here}}
  _ = x.c+x.b+x.w  // expected-warning 2 {{properties in classes always cause a copy to the accelerator}}

}

public enum X {
  case A, B
}

public func invalidAttributeArg() -> TensorHandle<Int32> {
  // expected-error@+1 {{attribute 'someAttr' requires a constant argument}}
  return #tfop("bar", someAttr: X.A)
}


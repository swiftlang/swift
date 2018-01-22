// RUN: %target-swift-frontend -emit-sil -verify %s

import TensorFlow


// Verify AccelerableTensorUnit makes it into the test.
func testAccelerableTensorUnit<T : AccelerableTensorUnit>(a : T) -> T {
  return a
}


func testTensor() {
  _ = Tensor<Float>(1.0)
  _ = Tensor<Int>(2)
  _ = Tensor<Int>([1.0, 2.0]) // expected-error {{cannot convert value of type '[Double]'}}
}

// Show inference of the tensor element type based on context.  This also
// exposed a SILGen bug handling cleanup generation when emitting into let
// declarations.
@_transparent
func testInferredElementResult() -> TensorHandle<Int> {
  // expected-warning @+1 {{immutable value 'x' was never used}}
  let x : TensorHandle<Int> = #tfop("foo", ":t")

  _ = #tfop("bar", ":t") as TensorHandle<Int>
}

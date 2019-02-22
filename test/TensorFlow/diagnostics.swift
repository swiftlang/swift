// RUN: %target-swift-frontend -Xllvm -tf-dynamic-compilation=false -Xllvm -tf-dump-intermediates -Xllvm -tf-warn-send-recv -O -emit-sil -verify %s

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

class ClassTest {
  var w = Tensor<Float>(zeros: [1, 2])  // expected-warning {{value implicitly copied to the host}}
  let b = Tensor<Float>(zeros: [1, 2])  // expected-warning {{value implicitly copied to the host}}

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

// b/76387659 - Verify that there is a way to configure the TPU.
// SR-9736: Fix this test in GPE/compiler mode.
// public func testDevice() {
//   TensorFlow.enableTPU()
//   let a = Tensor<Float>(1.0)
//   _ = a+a

//   // TODO: remove the extra code below once TPU execution supports 0 output
//   // tensors (b/111123797)
//   let extra = Tensor<Float>(1.0)
//   _hostOp(extra)
// }

// This loop is unrolled, so we have multiple SIL values for `x`, but there
// should be a single copy-to-host compiler warning.
public func SR8412_CopyToHost() {
  for _ in 0...10 {
    let x = Tensor(1)  // expected-warning {{value implicitly copied to the host}}
    _hostOp(x)
  }
}

@inline(never)
public func hostScalarTensor() -> Tensor<Float> {
  return Tensor<Float>(1.0)
}

@inline(never)
public func hostNonScalarTensor() -> Tensor<Float> {
  return Tensor<Float>([1.0, 2.0])
}

public func SR8412_CopyToAccel(a: Int32) {
  let x = Tensor<Float>(1.0)
  for _ in 0...10 {
    let _ = hostScalarTensor() + x  // expected-warning {{value implicitly copied to the accelerator}}
  }
}

public func copyScalarTensorToAccel() -> Tensor<Float> {
  return hostScalarTensor() + 1  // expected-warning {{value implicitly copied to the accelerator}}
}

public func copyNonScalarTensorToAccel() -> Tensor<Float> {
  return hostNonScalarTensor() + 1  // expected-warning {{value implicitly copied to the accelerator}}
}

// Test that we do not diagnose a scalar transfer to the accelerator. There is a parallel test in
// diagnostics_scalar_transfers.swift that tests that we do diagnose when
// tf-warn-scalar-transfer=true.
public func scalarToAccelerator(x: Float) -> Tensor<Float> {
  return Tensor(x) + 1
}

// Test that we do not diagnose a scalar transfer to the host. There is a parallel test in
// diagnostics_scalar_transfers.swift that tests that we do diagnose when
// tf-warn-scalar-transfer=true.
public func scalarToHost() {
  var i = Tensor(0)
  while i < 10 {
    print("Running loop body")
    i += 1
  }
}

// expected-warning @+1 {{'t' implicitly copied to the accelerator}}
public func inoutArgumentToAccelerator(t: inout Tensor<Float>) {
  t += 1  // expected-note {{value used here}}
}

// RUN: %target-typecheck-verify-swift

import TensorFlow

@TensorFlowGraph
func justHandles(_ x: TensorHandle<Int32>) -> TensorHandle<Double> {} // okay

@TensorFlowGraph
func justHandles(_ x: ResourceHandle) -> VariantHandle {} // okay

@TensorFlowGraph
func multiRetHandles(_ x: ResourceHandle, _ y: VariantHandle) -> (TensorHandle<Int32>, TensorHandle<Float>) {} // okay

@TensorFlowGraph
func tensorIn(_ x: Tensor<Float>) -> ResourceHandle {} // okay

@TensorFlowGraph
func tensorOut(_ x: ResourceHandle) -> Tensor<Float> {} // okay

@TensorFlowGraph
func tensors(_ x: Tensor<Float>) -> Tensor<Int32> {} // okay

@TensorFlowGraph
func multiRetTensors(_ x: Tensor<Int32>) -> (Tensor<Double>, Tensor<Float>) {} // okay

// expected-error @+1 {{@TensorFlowGraph cannot be applied to generic functions}}
@TensorFlowGraph
func generic<T>(_ x: Tensor<T>) -> Tensor<T> {}

// expected-error @+1 {{@TensorFlowGraph can only be applied to functions whose parameters and return values are TensorFlow values or aggregates of TensorFlow values}}
@TensorFlowGraph
func wrongInput(_ x: Int32) -> ResourceHandle {}

// expected-error @+1 {{@TensorFlowGraph can only be applied to functions whose parameters and return values are TensorFlow values or aggregates of TensorFlow values}}
@TensorFlowGraph
func wrongOutput(_ x: Tensor<Float>) -> Float {}

enum SomeType {
  // expected-error @+1 {{@TensorFlowGraph can only be applied to top-level functions}}
  @TensorFlowGraph
  func methodNotOkay(_ x: Tensor<Float>) -> Tensor<Float> {}

  // expected-error @+1 {{@TensorFlowGraph can only be applied to top-level functions}}
  @TensorFlowGraph
  static func methodNotOkay2(_ x: Tensor<Float>) -> Tensor<Float> {}
}

let f: @convention(tensorflow) (Tensor<Float>) -> Tensor<Int32> = tensors(_:) // okay
let g: (Tensor<Float>) -> Tensor<Int32> = tensors(_:) // expected-error {{TensorFlow functions cannot be converted to other function types}}

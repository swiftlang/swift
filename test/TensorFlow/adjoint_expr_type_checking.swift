// RUN: %target-swift-frontend -typecheck -verify %s

import TensorFlow

// Test overloading top-level `sin` function.
func sin(_ x: Float) -> Float { return x }

func testSpecialized(_ t: Tensor<Float>) {
  _ = #adjoint(relu)(t, originalValue: t, seed: t)
  _ = #adjoint(sin)(t, originalValue: t, seed: t)

  _ = #adjoint(Tensor<Float>.+)
  let _: (Tensor<Float>, Tensor<Float>, Tensor<Float>, Tensor<Float>) -> (Tensor<Float>, Tensor<Float>)
    = #adjoint(Tensor.+)

  _ = #adjoint(Tensor<Float>.convolved2D)
  let _: (Tensor<Float>) -> (Tensor<Float>, (Int32, Int32, Int32, Int32), Padding, Tensor<Float>, Tensor<Float>) -> (Tensor<Float>, Tensor<Float>)
    = #adjoint(Tensor.convolved2D)
}

func testGeneric<T : BinaryFloatingPoint>( _ t: Tensor<T>) {
  let _ = #adjoint(relu)(t, originalValue: t, seed: t)
  let _ = #adjoint(sin)(t, originalValue: t, seed: t)

  _ = #adjoint(Tensor<T>.+)
  let _: (Tensor<T>, Tensor<T>, Tensor<T>, Tensor<T>) -> (Tensor<T>, Tensor<T>)
    = #adjoint(Tensor.+)

  _ = #adjoint(Tensor<T>.convolved2D)
  let _: (Tensor<T>) -> (Tensor<T>, (Int32, Int32, Int32, Int32), Padding, Tensor<T>, Tensor<T>) -> (Tensor<T>, Tensor<T>)
    = #adjoint(Tensor.convolved2D)
}

func testLabels<T : BinaryFloatingPoint>(_ t: Tensor<T>) {
  // If the #adjoint expression is applied directly, argument labels are
  // necessary.
  // This matches the behavior for normal function application.
  _ = #adjoint(matmul)(t, t, originalValue: t, seed: t)
  _ = #adjoint(Tensor.+)(t, t, originalValue: t, seed: t)
  _ = #adjoint(Tensor.batchNormalized)(t)(
    alongAxis: 0, offset: 0, scale: 0, epsilon: 0, originalValue: t, seed: t
  )

  // If the #adjoint expression is unapplied and bound to a variable, argument
  // labels are not necessary.
  let dMatmul: (Tensor<T>, Tensor<T>, Tensor<T>, Tensor<T>) -> (Tensor<T>, Tensor<T>)
    = #adjoint(matmul)
  _ = dMatmul(t, t, t, t)
  let dAdd = #adjoint(Tensor<T>.+)
  _ = dAdd(t, t, t, t)
  let dBatchNorm = #adjoint(Tensor<T>.batchNormalized)
  _ = dBatchNorm(t)(0, 0, 0, 0, t, t)
}

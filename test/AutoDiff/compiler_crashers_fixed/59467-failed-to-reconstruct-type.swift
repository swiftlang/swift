// RUN: %target-swift-frontend -emit-ir -g -primary-file %s %S/Inputs/59467-failed-to-reconstruct-type-second.swift -module-name TensorFlow

import _Differentiation

@_semantics("autodiff.nonvarying")
func withoutDerivative() -> Tensor {
  fatalError()
}

func BatchNorm_doInference(
  _ input: Tensor
) -> Tensor {
  withoutDerivative()
}

@differentiable(reverse)
func BatchNorm_callAsFunction(_ input: Tensor) -> Tensor {
  BatchNorm_doInference(input)
}

@differentiable(reverse)
func LayerNorm_callAsFunction(_ input: Tensor) -> Tensor {
  rsqrt(input)
}

import _Differentiation

struct Tensor: Differentiable {}

// `Tensor` could be defined in this test case's primary file and the crash
// would still happen. All that matters is that `LayerNorm_callAsFunction` and
// `rsqrt` are defined in separate files.

@differentiable(reverse)
func rsqrt( _ x: Tensor) -> Tensor {
  fatalError()
}

@derivative(of: rsqrt)
func _vjpRsqrt(_ x: Tensor) -> (
  value: Tensor, pullback: (Tensor.TangentVector) -> (Tensor.TangentVector)
) {
  fatalError()
}

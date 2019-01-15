// RUN: %target-swift-frontend -emit-sil %s | %FileCheck %s

@differentiable(adjoint: adjointId)
func id(_ x: Float) -> Float {
  return x
}

func adjointId(_ seed: Float, _ originalValue: Float, _ x: Float) -> Float {
  return seed
}

_ = gradient(at: 2, in: id)

// CHECK-LABEL: @{{.*}}id{{.*}}__vjp_src_0_wrt_0

#if os(macOS) || os(iOS) || os(tvOS) || os(watchOS)
import func Darwin.exp
#else
import func Glibc.exp
#endif

@differentiable(primal: primalSigmoid, adjoint: adjointSigmoid)
func sigmoid(_ x: Double) -> Double {
  return 1.0 / (1.0 + exp(-x))
}

func primalSigmoid(_ x: Double) -> (checkpoints: (Double, Double, Double), result: Double) {
  let minusX = -x
  let expon = exp(minusX)
  let plus = 1.0 + expon
  let div = 1.0 / plus
  return (checkpoints: (minusX, expon, plus), result: div)
}

func adjointSigmoid(_ seed: Double, _ checkpoints: (Double, Double, Double), _ result: Double, _ x: Double) -> Double {
  return result * (1 - result)
}

let x = gradient(at: 3, in: sigmoid)
let (value: y, gradient: z) = valueWithGradient(at: 4, in: sigmoid)
print(x * z)

// CHECK-LABEL: @{{.*}}sigmoid{{.*}}__vjp_src_0_wrt_0

@differentiable()
public func publicFunc(_ x: Float) -> Float {
  return x + x
}

// CHECK-LABEL: @{{.*}}publicFunc{{.*}}__vjp_src_0_wrt_0
// CHECK: @{{.*}}publicFunc{{.*}}__primal_src_0_wrt_0
// CHECK: @{{.*}}publicFunc{{.*}}__adjoint_src_0_wrt_0

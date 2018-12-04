// RUN: %target-swift-frontend -emit-sil %s | %FileCheck %s

@differentiable(reverse, adjoint: adjointId)
func id(_ x: Float) -> Float {
  return x
}

func adjointId(_ seed: Float, _ originalValue: Float, _ x: Float) -> Float {
  return seed
}

_ = #gradient(id)(2)

// CHECK-LABEL: @{{.*}}id{{.*}}__grad_src_0_wrt_0
// CHECK-LABEL: @{{.*}}id{{.*}}__grad_src_0_wrt_0_s_p

#if os(macOS) || os(iOS) || os(tvOS) || os(watchOS)
import func Darwin.exp
#else
import func Glibc.exp
#endif

@differentiable(reverse, primal: primalSigmoid, adjoint: adjointSigmoid)
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

let x = #gradient(sigmoid)(3)
let (value: y, gradient: z) = #valueAndGradient(sigmoid)(4)
print(x * z)

// CHECK-LABEL: @{{.*}}sigmoid{{.*}}__grad_src_0_wrt_0
// CHECK: @{{.*}}sigmoid{{.*}}__grad_src_0_wrt_0_s_p
// CHECK: @{{.*}}sigmoid{{.*}}__grad_src_0_wrt_0_p


public func publicFunc(_ x: Float) -> Float {
  return x + x
}
_ = #gradient(publicFunc)

// CHECK-LABEL: @{{.*}}publicFunc{{.*}}__grad_src_0_wrt_0
// CHECK: @{{.*}}publicFunc{{.*}}__primal_src_0_wrt_0
// CHECK: @{{.*}}publicFunc{{.*}}__adjoint_src_0_wrt_0

// RUN: %target-swift-emit-sil %s | %FileCheck %s

import _Differentiation

// CHECK: sil private{{.*}}@$s17optional_pullback23givesWrongTangentVector1xxSgx_t16_Differentiation14DifferentiableRzlFAeFRzlTJpSpSr
// CHECK-SAME: $@convention(thin) <τ_0_0 where τ_0_0 : Differentiable>
// CHECK-SAME: (@in_guaranteed Optional<τ_0_0>.TangentVector) -> @out τ_0_0.TangentVector
//
// CHECK: bb0(%[[RET_TAN:.+]] : $*τ_0_0.TangentVector, %[[OPT_TAN:.+]] : $*Optional<τ_0_0>.TangentVector):
// CHECK: %[[RET_TAN_BUF:.+]] = alloc_stack $τ_0_0.TangentVector

// CHECK: %[[ZERO1:.+]] = witness_method $τ_0_0.TangentVector, #AdditiveArithmetic.zero!getter
// CHECK: apply %[[ZERO1]]<τ_0_0.TangentVector>(%[[RET_TAN_BUF]], %{{.*}})
//
// CHECK: %[[TAN_VAL_COPY:.+]] = alloc_stack $Optional<τ_0_0.TangentVector>
// CHECK: %[[TAN_BUF:.+]] = alloc_stack $Optional<τ_0_0>.TangentVector

// CHECK: copy_addr %[[OPT_TAN]] to [init] %[[TAN_BUF]] : $*Optional<τ_0_0>.TangentVector
// CHECK: %[[TAN_VAL:.+]] = struct_element_addr %[[TAN_BUF]] : $*Optional<τ_0_0>.TangentVector, #Optional.TangentVector.value
// CHECK: copy_addr %[[TAN_VAL]] to [init] %[[TAN_VAL_COPY]] : $*Optional<τ_0_0.TangentVector>
//
// CHECK: %[[TAN_DATA:.+]] = unchecked_take_enum_data_addr %[[TAN_VAL_COPY]] : $*Optional<τ_0_0.TangentVector>, #Optional.some!enumelt
// CHECK: %[[PLUS_EQUAL:.+]] = witness_method $τ_0_0.TangentVector, #AdditiveArithmetic."+="
// CHECK: apply %[[PLUS_EQUAL]]<τ_0_0.TangentVector>(%[[RET_TAN_BUF]], %[[TAN_DATA]], %{{.*}})
//
// CHECK: destroy_addr %[[TAN_DATA]] : $*τ_0_0.TangentVector
// CHECK: %[[ZERO2:.+]] = witness_method $τ_0_0.TangentVector, #AdditiveArithmetic.zero!getter
// CHECK: apply %[[ZERO2]]<τ_0_0.TangentVector>(%[[TAN_DATA]], %{{.*}})
// CHECK: destroy_addr %[[TAN_DATA]] : $*τ_0_0.TangentVector
//
// CHECK: copy_addr [take] %[[RET_TAN_BUF:.+]] to [init] %[[RET_TAN:.+]]
// CHECK: destroy_addr %[[TAN_BUF]] : $*Optional<τ_0_0>.TangentVector
// CHECK: dealloc_stack %[[TAN_BUF]] : $*Optional<τ_0_0>.TangentVector
// CHECK: dealloc_stack %[[TAN_VAL_COPY]] : $*Optional<τ_0_0.TangentVector>
// CHECK: dealloc_stack %[[RET_TAN_BUF]] : $*τ_0_0.TangentVector

@differentiable(reverse)
func givesWrongTangentVector<Element>(x: Element) -> Element? where Element: Differentiable {
    return x
}

@differentiable(reverse)
func f(x: Double) -> Double {
    let y = givesWrongTangentVector(x: x)
    return y!
}

print(valueWithGradient(at: 0.0, of: f))

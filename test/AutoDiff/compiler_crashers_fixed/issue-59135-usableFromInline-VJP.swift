// RUN: %empty-directory(%t)
// RUN: %target-swift-emit-sil -emit-module -module-name M -emit-module-path %t/M.swiftmodule 2>&1 %s | %FileCheck %s

// The original function Tensor.subscriptIndexPath() is not marked as @differentiable. As a result, no explicit differentiable witness is generated for it.
// However, the witness is generated as a side effect of providing a derivative via  @derivative(of: subscriptIndexPath) on _vjpSubscriptIndexPath.
// Since _vjpSubscriptIndexPath is not emitted when -emit-module is used, we need to ensure we still generate a witness.

import _Differentiation

// CHECK-LABEL: differentiability witness for Tensor.subscriptIndexPath()
// CHECK: sil_differentiability_witness [serialized] [reverse] [parameters 0] [results 0] @$s1M6TensorV18subscriptIndexPathACyF : $@convention(method) (Tensor) -> Tensor {
// CHECK:  vjp: @$s1M6TensorV18subscriptIndexPathACyFTJrSpSr : $@convention(method) (Tensor) -> (Tensor, @owned @callee_guaranteed (Tensor) -> Tensor)

// CHECK-LABEL: reverse-mode derivative of Tensor.subscriptIndexPath()
// CHECK: @$s1M6TensorV18subscriptIndexPathACyFTJrSpSr : $@convention(method) (Tensor) -> (Tensor, @owned @callee_guaranteed (Tensor) -> Tensor) {
// CHECK: function_ref Tensor._vjpSubscriptIndexPath()
// CHECK: function_ref @$s1M6TensorV22_vjpSubscriptIndexPathAC5value_A2Cc8pullbacktyF : $@convention(method) (Tensor) -> (Tensor, @owned @callee_guaranteed (Tensor) -> Tensor)

public struct Tensor: Differentiable & AdditiveArithmetic {
  @inlinable
  func subscriptIndexPath() -> Tensor {
    fatalError()
  }

  @inlinable
  @differentiable(reverse, wrt: self)
  func subscriptRanges() -> Tensor {
    subscriptIndexPath()
  }
  
  @usableFromInline
  @derivative(of: subscriptIndexPath)
  func _vjpSubscriptIndexPath() -> (
    value: Tensor, pullback: (Tensor) -> Tensor
  ) {
    fatalError()
  }
}

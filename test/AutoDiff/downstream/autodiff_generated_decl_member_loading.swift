// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module %s -o %t/autodiff_generated_decl_member_loading_cross_module.swiftmodule
// RUN: %target-swift-frontend -merge-modules -sil-merge-partial-modules -emit-module %t/autodiff_generated_decl_member_loading_cross_module.swiftmodule

// Tests TF-805.
//
// Previously, `IterableDeclContext::loadAllMembers` was disabled for
// AD-generated structs/enums:
// https://github.com/apple/swift/commit/7e89bee39bfda4624f04dcc2c8d53599fbde6191
//
// This caused a SIL verification failure for because enum members were not loaded
// when running `-emit-module` then `-merge-modules -sil-merge-partial-modules`:
//
//     SIL verification failed: switch_enum dispatches on same enum element
//     more than once: unswitchedElts.count(elt)

public struct Tensor<Scalar> {}

extension Tensor: Differentiable where Scalar: Differentiable{
  public typealias TangentVector = Float
  public mutating func move(along direction: Float) {}
}

extension Tensor {
  @inlinable
  @differentiable(wrt: self where Scalar: Differentiable)
  func TF_805(axis: Int) -> Tensor {
    if axis != axis {}
    return self
  }
}

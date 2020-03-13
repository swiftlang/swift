// RUN: %target-swift-emit-sil -enable-experimental-forward-mode-differentiation -verify -module-name main %s | %FileCheck %s

// Test derivative generic signatures:
// - In `@differentiable` and `@derivative` attributes.
// - In SIL differentiability witnesses.
// - In generated derivative functions and derivative function thunks.

//===----------------------------------------------------------------------===//
// Same-type requirements
//===----------------------------------------------------------------------===//

// If all generic parameters are concrete (e.g. bound via same-type requirements
// to concrete types), `@differentiable` attribute should not have a derivative
// generic signature.

// Test `@differentiable` attribute where original declaration has generic
// signature and all generic parameters are concrete (e.g. bound to concrete
// types via same-type requirements). SILGen lowers the original declaration to
// a function with no generic signature, so the differentiability witness should
// have no derivative generic signature.

// NOTE(SR-11950): SILParser crashes for SILGen round-trip.

// Test same-type requirements.

// If all generic parameters are concrete (e.g. bound via same-type requirements
// to concrete types), `@differentiable` attribute should have no derivative
// generic signature. Otherwise, :

struct AllConcrete<T>: Differentiable {}

extension AllConcrete {
  // Original generic signature: `<T>`.
  // Where clause generic signature: `<T where T == Float>`.
  @_silgen_name("allconcrete_where_gensig_constrained")
  @differentiable(where T == Float)
  func whereClauseGenericSignatureConstrained() -> AllConcrete {
    return self
  }

// CHECK-LABEL: // differentiability witness for allconcrete_where_gensig_constrained
// CHECK-NEXT: sil_differentiability_witness hidden [parameters 0] [results 0] <T where T == Float> @allconcrete_where_gensig_constrained : $@convention(method) <T> (AllConcrete<T>) -> AllConcrete<T> {
// CHECK-NEXT:   jvp: @AD__allconcrete_where_gensig_constrained__jvp_src_0_wrt_0_SfRszl : $@convention(method) (AllConcrete<Float>) -> (AllConcrete<Float>, @owned @callee_guaranteed (AllConcrete<Float>.TangentVector) -> AllConcrete<Float>.TangentVector)
// CHECK-NEXT:   vjp: @AD__allconcrete_where_gensig_constrained__vjp_src_0_wrt_0_SfRszl : $@convention(method) (AllConcrete<Float>) -> (AllConcrete<Float>, @owned @callee_guaranteed (AllConcrete<Float>.TangentVector) -> AllConcrete<Float>.TangentVector)
// CHECK-NEXT: }
}
extension AllConcrete where T == Float {
  @derivative(of: whereClauseGenericSignatureConstrained)
  func jvpWhereClauseGenericSignatureConstrained() -> (
    value: AllConcrete, differential: (TangentVector) -> TangentVector
  ) {
    (whereClauseGenericSignatureConstrained(), { $0 })
  }
}

extension AllConcrete where T == Float {
  // Original generic signature: `<T where T == Float>`.
  // Where clause generic signature: none.
  @_silgen_name("allconcrete_original_gensig")
  @differentiable
  func originalGenericSignature() -> AllConcrete {
    return self
  }

  @derivative(of: originalGenericSignature)
  func jvpOriginalGenericSignature() -> (
    value: AllConcrete, differential: (TangentVector) -> TangentVector
  ) {
    (originalGenericSignature(), { $0 })
  }

// CHECK-LABEL: // differentiability witness for allconcrete_original_gensig
// CHECK-NEXT: sil_differentiability_witness hidden [parameters 0] [results 0] @allconcrete_original_gensig : $@convention(method) (AllConcrete<Float>) -> AllConcrete<Float> {
// CHECK-NEXT:   jvp: @AD__allconcrete_original_gensig__jvp_src_0_wrt_0 : $@convention(method) (AllConcrete<Float>) -> (AllConcrete<Float>, @owned @callee_guaranteed (AllConcrete<Float>.TangentVector) -> AllConcrete<Float>.TangentVector)
// CHECK-NEXT:   vjp: @AD__allconcrete_original_gensig__vjp_src_0_wrt_0 : $@convention(method) (AllConcrete<Float>) -> (AllConcrete<Float>, @owned @callee_guaranteed (AllConcrete<Float>.TangentVector) -> AllConcrete<Float>.TangentVector)
// CHECK-NEXT: }

  // Original generic signature: `<T where T == Float>`.
  // Where clause generic signature: `<T where T == Float>`.
  @_silgen_name("allconcrete_where_gensig")
  @differentiable(where T == Float)
  func whereClauseGenericSignature() -> AllConcrete {
    return self
  }

  @derivative(of: whereClauseGenericSignature)
  func jvpWhereClauseGenericSignature() -> (
    value: AllConcrete, differential: (TangentVector) -> TangentVector
  ) {
    (whereClauseGenericSignature(), { $0 })
  }

// CHECK-LABEL: // differentiability witness for allconcrete_where_gensig
// CHECK-NEXT: sil_differentiability_witness hidden [parameters 0] [results 0] @allconcrete_where_gensig : $@convention(method) (AllConcrete<Float>) -> AllConcrete<Float> {
// CHECK-NEXT:   jvp: @AD__allconcrete_where_gensig__jvp_src_0_wrt_0 : $@convention(method) (AllConcrete<Float>) -> (AllConcrete<Float>, @owned @callee_guaranteed (AllConcrete<Float>.TangentVector) -> AllConcrete<Float>.TangentVector)
// CHECK-NEXT:   vjp: @AD__allconcrete_where_gensig__vjp_src_0_wrt_0 : $@convention(method) (AllConcrete<Float>) -> (AllConcrete<Float>, @owned @callee_guaranteed (AllConcrete<Float>.TangentVector) -> AllConcrete<Float>.TangentVector)
// CHECK-NEXT: }
}

extension AllConcrete where T == Float {
  func testDifferentiability() {
    let _: @differentiable (AllConcrete) -> AllConcrete =
      { $0.originalGenericSignature() }
    let _: @differentiable (AllConcrete) -> AllConcrete =
      { $0.whereClauseGenericSignature() }
    let _: @differentiable (AllConcrete) -> AllConcrete =
      { $0.whereClauseGenericSignatureConstrained() }
  }
}

// Test `@differentiable` attribute where original declaration has generic
// signature and not all generic parameters are concrete. The lowered SIL
// function and the differentiability witness should both have a derivative
// generic signature.

// NOTE(SR-11950): SILParser crashes for SILGen round-trip.

struct NotAllConcrete<T, U>: Differentiable {}

extension NotAllConcrete {
  // Original generic signature: `<T, U>`.
  // Where clause generic signature: `<T, U where T == Float>`.
  @_silgen_name("notallconcrete_where_gensig_constrained")
  @differentiable(where T == Float)
  func whereClauseGenericSignatureConstrained() -> NotAllConcrete {
    return self
  }

// CHECK-LABEL: // differentiability witness for notallconcrete_where_gensig_constrained
// CHECK-NEXT: sil_differentiability_witness hidden [parameters 0] [results 0] <T, U where T == Float> @notallconcrete_where_gensig_constrained : $@convention(method) <T, U> (NotAllConcrete<T, U>) -> NotAllConcrete<T, U> {
// CHECK-NEXT:   jvp: @AD__notallconcrete_where_gensig_constrained__jvp_src_0_wrt_0_SfRszr0_l : $@convention(method) <τ_0_0, τ_0_1 where τ_0_0 == Float> (NotAllConcrete<Float, τ_0_1>) -> (NotAllConcrete<Float, τ_0_1>, @owned @callee_guaranteed @substituted <τ_0_0, τ_0_1> (τ_0_0) -> τ_0_1 for <NotAllConcrete<Float, τ_0_1>.TangentVector, NotAllConcrete<Float, τ_0_1>.TangentVector>)
// CHECK-NEXT:   vjp: @AD__notallconcrete_where_gensig_constrained__vjp_src_0_wrt_0_SfRszr0_l : $@convention(method) <τ_0_0, τ_0_1 where τ_0_0 == Float> (NotAllConcrete<Float, τ_0_1>) -> (NotAllConcrete<Float, τ_0_1>, @owned @callee_guaranteed @substituted <τ_0_0, τ_0_1> (τ_0_0) -> τ_0_1 for <NotAllConcrete<Float, τ_0_1>.TangentVector, NotAllConcrete<Float, τ_0_1>.TangentVector>)
// CHECK-NEXT: }
}
extension NotAllConcrete where T == Float {
  @derivative(of: whereClauseGenericSignatureConstrained)
  func jvpWhereClauseGenericSignatureConstrained() -> (
    value: NotAllConcrete, differential: (TangentVector) -> TangentVector
  ) {
    (whereClauseGenericSignatureConstrained(), { $0 })
  }
}

extension NotAllConcrete where T == Float {
  // Original generic signature: `<T, U where T == Float>`.
  // Where clause generic signature: none.
  @_silgen_name("notallconcrete_original_gensig")
  @differentiable
  func originalGenericSignature() -> NotAllConcrete {
    return self
  }

  @derivative(of: originalGenericSignature)
  func jvpOriginalGenericSignature() -> (
    value: NotAllConcrete, differential: (TangentVector) -> TangentVector
  ) {
    (originalGenericSignature(), { $0 })
  }

// CHECK-LABEL: // differentiability witness for notallconcrete_original_gensig
// CHECK-NEXT: sil_differentiability_witness hidden [parameters 0] [results 0] <T, U where T == Float> @notallconcrete_original_gensig : $@convention(method) <T, U where T == Float> (NotAllConcrete<Float, U>) -> NotAllConcrete<Float, U> {
// CHECK-NEXT:   jvp: @AD__notallconcrete_original_gensig__jvp_src_0_wrt_0_SfRszr0_l : $@convention(method) <τ_0_0, τ_0_1 where τ_0_0 == Float> (NotAllConcrete<Float, τ_0_1>) -> (NotAllConcrete<Float, τ_0_1>, @owned @callee_guaranteed @substituted <τ_0_0, τ_0_1> (τ_0_0) -> τ_0_1 for <NotAllConcrete<Float, τ_0_1>.TangentVector, NotAllConcrete<Float, τ_0_1>.TangentVector>)
// CHECK-NEXT:   vjp: @AD__notallconcrete_original_gensig__vjp_src_0_wrt_0_SfRszr0_l : $@convention(method) <τ_0_0, τ_0_1 where τ_0_0 == Float> (NotAllConcrete<Float, τ_0_1>) -> (NotAllConcrete<Float, τ_0_1>, @owned @callee_guaranteed @substituted <τ_0_0, τ_0_1> (τ_0_0) -> τ_0_1 for <NotAllConcrete<Float, τ_0_1>.TangentVector, NotAllConcrete<Float, τ_0_1>.TangentVector>)
// CHECK-NEXT: }

  // Original generic signature: `<T, U where T == Float>`.
  // Where clause generic signature: `<T, U where T == Float>`.
  @_silgen_name("notallconcrete_where_gensig")
  @differentiable(where T == Float)
  func whereClauseGenericSignature() -> NotAllConcrete {
    return self
  }

  @derivative(of: whereClauseGenericSignature)
  func jvpWhereClauseGenericSignature() -> (
    value: NotAllConcrete, differential: (TangentVector) -> TangentVector
  ) {
    (whereClauseGenericSignature(), { $0 })
  }

// CHECK-LABEL: // differentiability witness for notallconcrete_where_gensig
// CHECK-NEXT: sil_differentiability_witness hidden [parameters 0] [results 0] <T, U where T == Float> @notallconcrete_where_gensig : $@convention(method) <T, U where T == Float> (NotAllConcrete<Float, U>) -> NotAllConcrete<Float, U> {
// CHECK-NEXT:   jvp: @AD__notallconcrete_where_gensig__jvp_src_0_wrt_0_SfRszr0_l : $@convention(method) <τ_0_0, τ_0_1 where τ_0_0 == Float> (NotAllConcrete<Float, τ_0_1>) -> (NotAllConcrete<Float, τ_0_1>, @owned @callee_guaranteed @substituted <τ_0_0, τ_0_1> (τ_0_0) -> τ_0_1 for <NotAllConcrete<Float, τ_0_1>.TangentVector, NotAllConcrete<Float, τ_0_1>.TangentVector>)
// CHECK-NEXT:   vjp: @AD__notallconcrete_where_gensig__vjp_src_0_wrt_0_SfRszr0_l : $@convention(method) <τ_0_0, τ_0_1 where τ_0_0 == Float> (NotAllConcrete<Float, τ_0_1>) -> (NotAllConcrete<Float, τ_0_1>, @owned @callee_guaranteed @substituted <τ_0_0, τ_0_1> (τ_0_0) -> τ_0_1 for <NotAllConcrete<Float, τ_0_1>.TangentVector, NotAllConcrete<Float, τ_0_1>.TangentVector>)
// CHECK-NEXT: }
}

extension NotAllConcrete where T == Float {
  func testDifferentiability() {
    let _: @differentiable (NotAllConcrete) -> NotAllConcrete =
      { $0.originalGenericSignature() }
    let _: @differentiable (NotAllConcrete) -> NotAllConcrete =
      { $0.whereClauseGenericSignature() }
    let _: @differentiable (NotAllConcrete) -> NotAllConcrete =
      { $0.whereClauseGenericSignatureConstrained() }
  }
}

// RUN: %target-build-swift %s
// REQUIRES: asserts

// SR-12548: SIL verification error regarding protocol witness thunks
// (`SILGenFunction::emitProtocolWitness`) for derivative function witnesses.

import _Differentiation

protocol P {}

struct Tensor<Scalar>: Equatable {
  @differentiable(where Scalar: P)
  static func +(lhs: Self, rhs: Self) -> Self { return lhs }
}
extension Tensor: Differentiable where Scalar: P {}

protocol Addable: Differentiable {
  @differentiable
  static func +(lhs: Self, rhs: Self) -> Self
}
extension Tensor: Addable where Scalar: P {}

// Original error:
// substitution map's generic signature: <Scalar>
// callee's generic signature: <τ_0_0 where τ_0_0 : P>
// SIL verification failed: Substitution map does not match callee in apply instruction: false
// Verifying instruction:
//      %4 = load [trivial] %1 : $*Tensor<τ_0_0>    // user: %10
//      %5 = load [trivial] %2 : $*Tensor<τ_0_0>    // user: %10
//      %6 = metatype $@thin Tensor<τ_0_0>.Type     // user: %10
//      %9 = differentiable_function_extract [jvp] %8 : $@differentiable @convention(method) <τ_0_0> (Tensor<τ_0_0>, Tensor<τ_0_0>, @noDerivative @thin Tensor<τ_0_0>.Type) -> Tensor<τ_0_0> // user: %10
// ->   %10 = apply %9<τ_0_0>(%4, %5, %6) : $@convention(method) <τ_0_0 where τ_0_0 : P> (Tensor<τ_0_0>, Tensor<τ_0_0>, @thin Tensor<τ_0_0>.Type) -> (Tensor<τ_0_0>, @owned @callee_guaranteed @substituted <τ_0_0, τ_0_1, τ_0_2> (τ_0_0, τ_0_1) -> τ_0_2 for <Tensor<τ_0_0>.TangentVector, Tensor<τ_0_0>.TangentVector, Tensor<τ_0_0>.TangentVector>) // user: %11
//      (%11, %12) = destructure_tuple %10 : $(Tensor<τ_0_0>, @callee_guaranteed @substituted <τ_0_0, τ_0_1, τ_0_2> (τ_0_0, τ_0_1) -> τ_0_2 for <Tensor<τ_0_0>.TangentVector, Tensor<τ_0_0>.TangentVector, Tensor<τ_0_0>.TangentVector>) // users: %13, %14
// In function:
// // AD__$s4main6TensorVyxGAA7AddableA2A1PRzlAaEP1poiyxx_xtFZTW_jvp_SSU
// sil private [transparent] [thunk] [ossa] @AD__$s4main6TensorVyxGAA7AddableA2A1PRzlAaEP1poiyxx_xtFZTW_jvp_SSU : $@convention(witness_method: Addable) <τ_0_0 where τ_0_0 : P> (@in_guaranteed Tensor<τ_0_0>, @in_guaranteed Tensor<τ_0_0>, @thick Tensor<τ_0_0>.Type) -> (@out Tensor<τ_0_0>, @owned @callee_guaranteed @substituted <τ_0_0, τ_0_1, τ_0_2> (@in_guaranteed τ_0_0, @in_guaranteed τ_0_1) -> @out τ_0_2 for <Tensor<τ_0_0>.TangentVector, Tensor<τ_0_0>.TangentVector, Tensor<τ_0_0>.TangentVector>) {
// ...

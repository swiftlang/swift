// {"frontendArgs":["-emit-sil","-enable-upcoming-feature","NonisolatedNonsendingByDefault"],"kind":"custom","signature":"getAutoDiffTangentTypeForLinearMap(swift::Type, llvm::function_ref<swift::ProtocolConformanceRef (swift::InFlightSubstitution&, swift::Type, swift::ProtocolDecl*)>, llvm::SmallVectorImpl<swift::GenericTypeParamType*>&, llvm::SmallVectorImpl<swift::Type>&, swift::ASTContext&)","signatureAssert":"Assertion failed: (maybeTanType && \"Type does not have a tangent space?\"), function getAutoDiffTangentTypeForLinearMap","signatureNext":"SILFunctionType::getAutoDiffDerivativeFunctionType"}
// RUN: not --crash %target-swift-frontend -emit-sil -enable-upcoming-feature NonisolatedNonsendingByDefault %s
// REQUIRES: OS=macosx
// REQUIRES: differentiable_programming
// REQUIRES: swift_feature_NonisolatedNonsendingByDefault
import _Differentiation
@differentiable(reverse)
func f(_ x: Float) async -> Float {
  x
}

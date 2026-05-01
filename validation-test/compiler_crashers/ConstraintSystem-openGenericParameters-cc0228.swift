// {"extraArgs":["-experimental-allow-module-with-compiler-errors","-emit-module-path","/dev/null"],"kind":"emit-sil","original":"f4b1cc69","signature":"swift::constraints::ConstraintSystem::openGenericParameters(swift::DeclContext*, swift::GenericSignature, llvm::SmallVectorImpl<std::__1::pair<swift::GenericTypeParamType*, swift::TypeVariableType*>>&, swift::constraints::ConstraintLocatorBuilder, swift::constraints::PreparedOverloadBuilder*)","signatureAssert":"Assertion failed: (!type->hasTypeParameter() && \"no generic environment provided for type with type parameters\"), function mapTypeIntoEnvironment","signatureNext":"ConstraintSystem::getTypeOfMemberReferencePre"}
// RUN: not --crash %target-swift-frontend -emit-sil -experimental-allow-module-with-compiler-errors -emit-module-path /dev/null %s
protocol a {
  struct b<c> {
    d {
    class e {
    init extension {
    actor f { g = e(
      }
    }

// {"kind":"typecheck","original":"0639ef40","signature":"swift::constraints::ConstraintSystem::openGenericParameters(swift::DeclContext*, swift::GenericSignature, llvm::SmallVectorImpl<std::__1::pair<swift::GenericTypeParamType*, swift::TypeVariableType*>>&, swift::constraints::ConstraintLocatorBuilder, swift::constraints::PreparedOverloadBuilder*)","signatureAssert":"Assertion failed: (!type->hasTypeParameter() && \"no generic environment provided for type with type parameters\"), function mapTypeIntoContext"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a {
  struct b {
      c<d
    {
      struct e<f
        extension Never {
          struct g {
      body {
        e

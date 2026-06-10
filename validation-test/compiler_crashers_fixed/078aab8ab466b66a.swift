// {"kind":"typecheck","signature":"swift::constraints::ConstraintSystem::openGenericParameters(swift::DeclContext*, swift::GenericSignature, llvm::SmallVectorImpl<std::__1::pair<swift::GenericTypeParamType*, swift::TypeVariableType*>>&, swift::constraints::ConstraintLocatorBuilder, swift::constraints::PreparedOverloadBuilder*)","signatureAssert":"Assertion failed: (sig), function openGenericParameters"}
// RUN: not %target-swift-frontend -typecheck %s
class a<b
  func c -> d
  typealias c  :  a
c

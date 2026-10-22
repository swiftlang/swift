// {"kind":"typecheck","original":"6e824970","signature":"matchCallArguments(swift::constraints::ConstraintSystem&, swift::FunctionType*, swift::ArgumentList*, llvm::ArrayRef<swift::AnyFunctionType::Param>, llvm::ArrayRef<swift::AnyFunctionType::Param>, swift::constraints::ConstraintKind, swift::constraints::ConstraintLocatorBuilder, std::__1::optional<swift::constraints::TrailingClosureMatching>, llvm::SmallVectorImpl<std::__1::pair<swift::TypeVariableType*, swift::ExistentialArchetypeType*>>&)","signatureAssert":"Assertion failed: (detail::isPresent(Val) && \"dyn_cast on a non-existent value\"), function dyn_cast","signatureNext":"ConstraintSystem::simplifyApplicableFnConstraint"}
// RUN: not --crash %target-swift-frontend -typecheck %s
{
  $1
}($a: [])

// {"kind":"typecheck","original":"0f807df4","signature":"matchCallArguments(swift::constraints::ConstraintSystem&, swift::FunctionType*, swift::ArgumentList*, llvm::ArrayRef<swift::AnyFunctionType::Param>, llvm::ArrayRef<swift::AnyFunctionType::Param>, swift::constraints::ConstraintKind, swift::constraints::ConstraintLocatorBuilder, std::__1::optional<swift::constraints::TrailingClosureMatching>, llvm::SmallVectorImpl<std::__1::pair<swift::TypeVariableType*, swift::ExistentialArchetypeType*>>&)","signatureAssert":"Assertion failed: (!HasOriginalArgs && \"Query original args instead\"), function getFirstTrailingClosureIndex","signatureNext":"ConstraintSystem::simplifyApplicableFnConstraint"}
// RUN: not --crash %target-swift-frontend -typecheck %s
macro
@freestanding(declaration arbitrary) macro a(Int...)
#a

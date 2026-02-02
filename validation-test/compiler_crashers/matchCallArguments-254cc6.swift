// {"kind":"typecheck","original":"ce7c1d7b","signature":"matchCallArguments(swift::constraints::ConstraintSystem&, swift::FunctionType*, swift::ArgumentList*, llvm::ArrayRef<swift::AnyFunctionType::Param>, llvm::ArrayRef<swift::AnyFunctionType::Param>, swift::constraints::ConstraintKind, swift::constraints::ConstraintLocatorBuilder, std::__1::optional<swift::constraints::TrailingClosureMatching>, llvm::SmallVectorImpl<std::__1::pair<swift::TypeVariableType*, swift::ExistentialArchetypeType*>>&)","signatureAssert":"Assertion failed: (param), function matchCallArguments"}
// RUN: not --crash %target-swift-frontend -typecheck %s
func a<each b>(c: (repeat each b) -> Void) {
  c($c: d)
}

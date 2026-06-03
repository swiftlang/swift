// {"kind":"typecheck","original":"eba97db4","signature":"matchCallArguments(swift::constraints::ConstraintSystem&, swift::FunctionType*, swift::ArgumentList*, llvm::ArrayRef<swift::AnyFunctionType::Param>, llvm::ArrayRef<swift::AnyFunctionType::Param>, swift::constraints::ConstraintKind, swift::constraints::ConstraintLocatorBuilder, std::__1::optional<swift::constraints::TrailingClosureMatching>, llvm::SmallVectorImpl<std::__1::pair<swift::TypeVariableType*, swift::ExistentialArchetypeType*>>&)","signatureAssert":"Assertion failed: (params.size() == argList->size()), function relabelParams","signatureNext":"ConstraintSystem::simplifyApplicableFnConstraint"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@dynamicMemberLookup struct a<b {
  g: b
  subscript<c>(dynamicMember d: KeyPath<b, c>)  c
  func callAsFunction -> c
}
let e = a(g: Double
let f = e(

// {"kind":"typecheck","original":"c9b1a7f7","signature":"matchCallArguments(swift::constraints::ConstraintSystem&, swift::FunctionType*, swift::ArgumentList*, llvm::ArrayRef<swift::AnyFunctionType::Param>, llvm::ArrayRef<swift::AnyFunctionType::Param>, swift::constraints::ConstraintKind, swift::constraints::ConstraintLocatorBuilder, std::__1::optional<swift::constraints::TrailingClosureMatching>, llvm::SmallVectorImpl<std::__1::pair<swift::TypeVariableType*, swift::ExistentialArchetypeType*>>&)","signatureAssert":"Assertion failed: (Index < this->size() && \"Invalid index!\"), function operator[]","signatureNext":"ConstraintSystem::simplifyApplicableFnConstraint"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@dynamicMemberLookup struct a<each b> {
  subscript<each c>(dynamicMember d: String)  (repeat each b repeat each c)-> Bool {
    a.e {
    }
  }
}

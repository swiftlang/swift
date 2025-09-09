// {"signature":"swift::constraints::ConstraintSystem::getTypeOfMemberReference(swift::Type, swift::ValueDecl*, swift::DeclContext*, bool, swift::FunctionRefInfo, swift::constraints::ConstraintLocator*, llvm::SmallVectorImpl<std::__1::pair<swift::GenericTypeParamType*, swift::TypeVariableType*>>*)"}
// RUN: not %target-swift-frontend -typecheck %s
struct a < let b > {
  func
  < (c : Self) {
    c.b
  }
}

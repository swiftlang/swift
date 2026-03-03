// {"kind":"typecheck","signature":"swift::constraints::ConstraintSystem::getTypeOfMemberReference(swift::Type, swift::ValueDecl*, swift::DeclContext*, bool, swift::FunctionRefInfo, swift::constraints::ConstraintLocator*, llvm::SmallVectorImpl<std::__1::pair<swift::GenericTypeParamType*, swift::TypeVariableType*>>*, swift::constraints::PreparedOverloadBuilder*)"}
// RUN: not %target-swift-frontend -typecheck %s
class a {
  macro b()
  var c = b
}

// {"kind":"typecheck","original":"2ce52aa0","signature":"swift::constraints::ConstraintSystem::recordOpenedType(swift::constraints::ConstraintLocator*, llvm::ArrayRef<std::__1::pair<swift::GenericTypeParamType*, swift::TypeVariableType*>>, swift::constraints::PreparedOverloadBuilder*)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
{
  switch
  0 {
  case .a( ... b

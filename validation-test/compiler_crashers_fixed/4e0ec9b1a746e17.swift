// {"signature":"swift::constraints::ConstraintSystem::recordOpenedTypes(swift::constraints::ConstraintLocatorBuilder, llvm::SmallVectorImpl<std::__1::pair<swift::GenericTypeParamType*, swift::TypeVariableType*>>&, bool)"}
// RUN: not %target-swift-frontend -typecheck %s
a<b> class a func a < c class b < d

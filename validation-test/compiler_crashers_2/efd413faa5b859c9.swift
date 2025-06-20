// {"signature":"swift::constraints::ConstraintSystem::recordOpenedTypes(swift::constraints::ConstraintLocatorBuilder, llvm::SmallVectorImpl<std::__1::pair<swift::GenericTypeParamType*, swift::TypeVariableType*>>&, bool)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct b let c = { (a : b)in switch a { case .d(... e

// {"kind":"typecheck","signature":"swift::constraints::ConstraintSystem::recordOpenedType(swift::constraints::ConstraintLocator*, llvm::ArrayRef<std::__1::pair<swift::GenericTypeParamType*, swift::TypeVariableType*>>, swift::constraints::PreparedOverloadBuilder*)","signatureAssert":"Assertion failed: (inserted), function recordOpenedType"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct b let c = { (a : b)in switch a { case .d(... e

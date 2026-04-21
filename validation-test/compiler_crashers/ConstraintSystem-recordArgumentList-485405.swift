// {"kind":"typecheck","original":"a32378f1","signature":"swift::constraints::ConstraintSystem::recordArgumentList(swift::constraints::ConstraintLocator*, swift::ArgumentList*)","signatureAssert":"Assertion failed: (inserted), function recordArgumentList","signatureNext":"ConstraintGenerator::visitKeyPathExpr"}
// RUN: not --crash %target-swift-frontend -typecheck %s
a -> switch  {
case .b(\b()

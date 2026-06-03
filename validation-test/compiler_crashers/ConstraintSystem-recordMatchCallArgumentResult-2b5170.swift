// {"kind":"typecheck","signature":"swift::constraints::ConstraintSystem::recordMatchCallArgumentResult(swift::constraints::ConstraintLocator*, swift::constraints::MatchCallArgumentResult)","signatureAssert":"Assertion failed: (inserted), function recordMatchCallArgumentResult","signatureNext":"ConstraintGenerator::visitUnresolvedDotExpr"}
// RUN: not --crash %target-swift-frontend -typecheck %s
if
case.a(a.b) = {

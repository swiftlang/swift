// {"kind":"typecheck","original":"702452ac","signature":"(anonymous namespace)::MultiConformanceChecker::checkAllConformances()","signatureAssert":"Assertion failed: (isa<SubscriptDecl>(Requirement) && \"Unhandled requirement kind\"), function getProtocolRequirementKind","signatureNext":"TypeChecker::checkConformancesInContext"}
// RUN: not --crash %target-swift-frontend -typecheck %s
class a: b  protocol b { macro a

// {"kind":"typecheck","original":"702452ac","signature":"(anonymous namespace)::MultiConformanceChecker::checkAllConformances()","signatureAssert":"Assertion failed: (isa<SubscriptDecl>(Requirement) && \"Unhandled requirement kind\"), function getProtocolRequirementKind"}
// RUN: not --crash %target-swift-frontend -typecheck %s
class a: b  protocol b { macro a

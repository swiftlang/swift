// {"kind":"typecheck","original":"902eebc9","signature":"diagnoseMatch(swift::ModuleDecl*, swift::NormalProtocolConformance*, swift::ValueDecl*, swift::RequirementMatch const&)","signatureAssert":"Assertion failed: (isa<SubscriptDecl>(Requirement) && \"Unhandled requirement kind\"), function getProtocolRequirementKind","signatureNext":"MultiConformanceChecker::checkAllConformances"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a {
  macro b()
  struct c: a {
    b
  }
}

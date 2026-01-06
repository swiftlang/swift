// {"kind":"complete","original":"0b556db7","signature":"diagnoseMatch(swift::ModuleDecl*, swift::NormalProtocolConformance*, swift::ValueDecl*, swift::RequirementMatch const&)","signatureAssert":"Assertion failed: (isa<SubscriptDecl>(Requirement) && \"Unhandled requirement kind\"), function getProtocolRequirementKind"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
protocol a {
  #^^#  macro b
  struct c: a {
b

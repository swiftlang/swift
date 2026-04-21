// {"kind":"complete","original":"fc52bd14","signature":"swift::constraints::ConstraintSystem::getType(swift::ASTNode) const","signatureAssert":"Assertion failed: (found != NodeTypes.end() && \"Expected type to have been set!\"), function getType","signatureNext":"ConstraintSystem::getCalleeLocator"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
func a(b: Bool??) {
  ~switch b {
  case .some(.some(#^^#.)):
  }
}

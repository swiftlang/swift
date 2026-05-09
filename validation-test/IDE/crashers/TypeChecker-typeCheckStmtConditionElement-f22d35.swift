// {"extraArgs":["-language-mode","6"],"kind":"complete","original":"bb63e562","signature":"swift::TypeChecker::typeCheckStmtConditionElement(swift::StmtConditionElement&, bool&, swift::DeclContext*)","signatureAssert":"Assertion failed: (!elt.getPattern()->hasType() && \"the pattern binding condition is already type checked\"), function typeCheckPatternBindingStmtConditionElement","signatureNext":"typeCheckConditionForStatement"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -swift-version 6 -source-filename %s
class a {
  b = {
    func c {
      guard let d
        #^^#

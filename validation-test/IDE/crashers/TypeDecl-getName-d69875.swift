// {"kind":"complete","original":"92d5efa4","signature":"swift::TypeDecl::getName() const","signatureAssert":"Assertion failed: ((closure->getParent() == DC || closure->getParent()->isChildContextOf(DC)) && \"Decl context isn't correct\"), function walkToClosureExprPre"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
extension {
  macro a = {
    #^^#

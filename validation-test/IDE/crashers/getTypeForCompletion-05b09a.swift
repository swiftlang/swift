// {"kind":"complete","original":"4c5d4dbc","signature":"swift::ide::getTypeForCompletion(swift::constraints::Solution const&, swift::ASTNode)","signatureAssert":"Assertion failed: (false && \"Expression wasn't type checked?\"), function getTypeForCompletion","signatureNext":"UnresolvedMemberTypeCheckCompletionCallback::sawSolutionImpl"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
a ? {
  func b {
    . #^^#

// {"kind":"complete","original":"9c6c0778","signature":"swift::ide::getTypeForCompletion(swift::constraints::Solution const&, swift::ASTNode)","signatureAssert":"Assertion failed: (false && \"Expression wasn't type checked?\"), function getTypeForCompletion","signatureNext":"AfterPoundExprCompletion::sawSolutionImpl"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
#imageLiteral(##^^#

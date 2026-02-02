// {"kind":"complete","original":"0517ff70","signature":"swift::ast_scope::ASTScopeImpl::getCharSourceRangeOfScope(swift::SourceManager&, bool) const","signatureAssert":"Assertion failed: ((closureExpr->getInLoc().isValid()) && \"We don't create these if no in loc\"), function getSourceRangeOfThisASTNode"}
// RUN: %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
{
  [a]
  b -> #^^#
}

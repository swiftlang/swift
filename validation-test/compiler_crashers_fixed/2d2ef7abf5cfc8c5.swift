// {"kind":"typecheck","original":"1a6377c6","signature":"swift::ast_scope::ASTScopeImpl::getCharSourceRangeOfScope(swift::SourceManager&, bool) const","signatureAssert":"Assertion failed: ((closureExpr->getInLoc().isValid()) && \"We don't create these if no in loc\"), function getSourceRangeOfThisASTNode"}
// RUN: not %target-swift-frontend -typecheck %s
{
  [a]
  b -> c
}

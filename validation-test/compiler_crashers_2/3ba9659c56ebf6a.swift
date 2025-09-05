// {"kind":"typecheck","signature":"swift::ast_scope::ASTScopeImpl::checkSourceRangeBeforeAddingChild(swift::ast_scope::ASTScopeImpl*, swift::ASTContext const&) const","signatureAssert":"Assertion failed: ((SM.isBefore(range.Start, range.End) || range.Start == range.End) && \"scope source range ends before start\"), function getCharSourceRangeOfScope"}
// RUN: not --crash %target-swift-frontend -typecheck %s
{
  @in

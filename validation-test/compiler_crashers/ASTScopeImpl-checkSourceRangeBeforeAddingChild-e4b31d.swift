// {"kind":"typecheck","original":"cb3e622a","signature":"swift::ast_scope::ASTScopeImpl::checkSourceRangeBeforeAddingChild(swift::ast_scope::ASTScopeImpl*, swift::ASTContext const&) const"}
// RUN: not --crash %target-swift-frontend -typecheck %s
{
  let a,
    () {
    }
}

// {"kind":"typecheck","original":"cb3e622a","signature":"swift::ast_scope::ASTScopeImpl::addChild(swift::ast_scope::ASTScopeImpl*, swift::ASTContext&)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
{
  let a,
    () {
    }
}

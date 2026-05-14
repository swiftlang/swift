// {"kind":"typecheck","original":"a5a8eb8a","signature":"swift::ast_scope::ASTScopeImpl::addChild(swift::ast_scope::ASTScopeImpl*, swift::ASTContext&)","signatureNext":"ScopeCreator::addExprToScopeTree::NestedExprScopeFinder::walkToExprPre"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a {
  var b: c = "\(try { try d(e: try { var i = d.a ) try i.a ; return i } 0) }0)"})"
}

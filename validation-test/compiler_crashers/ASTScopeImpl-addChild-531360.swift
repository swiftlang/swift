// {"kind":"typecheck","original":"a5a8eb8a","signature":"swift::ast_scope::ASTScopeImpl::addChild(swift::ast_scope::ASTScopeImpl*, swift::ASTContext&)","signatureNext":"ScopeCreator::addExprToScopeTree::NestedExprScopeFinder::walkToExprPre"}
// The crash depends on the C++ parser's handling of this input.
// UNSUPPORTED: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a {
  var b: c = "\(try { try d(e: try { var i = d.a ) try i.a ; return i } 0) }0)"})"
}

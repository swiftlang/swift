// {"kind":"complete","original":"081c2cc3","signature":"swift::ast_scope::ASTScopeImpl::addChild(swift::ast_scope::ASTScopeImpl*, swift::ASTContext&)","signatureNext":"ScopeCreator::constructExpandAndInsert"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
protocol a {
  associatedtype b: SignedInteger
    c,
    : {
    }
}
extension a {
  d: SignedInteger, #^^#

// {"kind":"complete","original":"0591f0bf","signature":"swift::ast_scope::ASTScopeImpl::addChild(swift::ast_scope::ASTScopeImpl*, swift::ASTContext&)","signatureNext":"ScopeCreator::constructWithPortionExpandAndInsert","useSourceOrderCompletion":true}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-order-completion -source-filename %s
func a {
for #else #^^#}
struct b func c {
#^d^#

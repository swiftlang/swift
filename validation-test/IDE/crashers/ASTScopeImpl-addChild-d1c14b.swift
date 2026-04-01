// {"kind":"complete","original":"26fa3db2","signature":"swift::ast_scope::ASTScopeImpl::addChild(swift::ast_scope::ASTScopeImpl*, swift::ASTContext&)","signatureNext":"GenericTypeOrExtensionWholePortion::expandScope","useSourceOrderCompletion":true}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-order-completion -source-filename %s
protocol a { associatedtype b: c
#^^#
#else
extension Array where #^d^#Element: a

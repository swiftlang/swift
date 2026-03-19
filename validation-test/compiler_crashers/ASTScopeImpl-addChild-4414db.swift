// {"kind":"typecheck","signature":"swift::ast_scope::ASTScopeImpl::addChild(swift::ast_scope::ASTScopeImpl*, swift::ASTContext&)","signatureNext":"ScopeCreator::addToScopeTreeAndReturnInsertionPoint"}
// RUN: not --crash %target-swift-frontend -typecheck %s
func a < >>

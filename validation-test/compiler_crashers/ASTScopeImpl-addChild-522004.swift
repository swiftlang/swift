// {"kind":"typecheck","original":"1c9cb9fd","signature":"swift::ast_scope::ASTScopeImpl::addChild(swift::ast_scope::ASTScopeImpl*, swift::ASTContext&)","signatureNext":"ScopeCreator::constructWithPortionExpandAndInsert"}
// RUN: not --crash %target-swift-frontend -typecheck %s
if { }
else extension {

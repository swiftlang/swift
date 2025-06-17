// {"signature":"swift::ast_scope::NodeAdder::visitBraceStmt(swift::BraceStmt*, swift::ast_scope::ASTScopeImpl*, swift::ast_scope::ScopeCreator&)::'lambda'(swift::ValueDecl*)::operator()(swift::ValueDecl*) const"}
// RUN: not --crash %target-swift-frontend -typecheck %s
{
  @abi(var a) subscript->

// {"kind":"typecheck","original":"3391810c","signature":"swift::ast_scope::ASTScopeImpl::addChild(swift::ast_scope::ASTScopeImpl*, swift::ASTContext&)","signatureNext":"PatternEntryDeclScope::expandAScopeThatCreatesANewInsertionPoint"}
// RUN: not --crash %target-swift-frontend -typecheck %s
extension a {
  b { "\( { 0 > String in struct d return "\({ var c = ({) )" }0 )"
  }

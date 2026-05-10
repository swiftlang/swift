// {"kind":"typecheck","languageMode":6,"original":"a266481a","signature":"swift::ast_scope::ASTScopeImpl::addChild(swift::ast_scope::ASTScopeImpl*, swift::ASTContext&)","signatureNext":"CaseStmtScope::expandAScopeThatDoesNotCreateANewInsertionPoint"}
// RUN: not --crash %target-swift-frontend -typecheck -swift-version 6 %s
switch { case where ./

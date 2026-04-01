// {"kind":"complete","original":"265e5934","signature":"swift::ast_scope::ASTScopeImpl::getCharSourceRangeOfScope(swift::SourceManager&, bool) const","signatureAssert":"Assertion failed: ((range.isValid()) && \"scope has invalid source range\"), function getCharSourceRangeOfScope","signatureNext":"ASTScopeImpl::checkSourceRangeBeforeAddingChild"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
let a { @b#^^#

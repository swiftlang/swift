// {"kind":"complete","signature":"swift::ast_scope::ASTScopeImpl::getCharSourceRangeOfScope(swift::SourceManager&, bool) const","signatureAssert":"Assertion failed: ((range.isValid()) && \"scope has invalid source range\"), function getCharSourceRangeOfScope"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
var a: b {
  @ #^COMPLETE^#

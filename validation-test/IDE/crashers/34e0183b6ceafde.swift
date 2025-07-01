// {"kind":"complete","signature":"swift::ast_scope::ASTScopeImpl::getCharSourceRangeOfScope(swift::SourceManager&, bool) const"}
// RUN: not --crash %target-swift-ide-test -code-completion --code-completion-token=COMPLETE -source-filename %s
var a: b {
  @ #^COMPLETE^#

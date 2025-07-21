// {"kind":"typecheck","signature":"swift::ast_scope::ASTScopeImpl::printRange(llvm::raw_ostream&) const"}
// RUN: not --crash %target-swift-frontend -typecheck %s
extension a {}
func b < >>

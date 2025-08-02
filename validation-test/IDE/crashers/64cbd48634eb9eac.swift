// {"kind":"complete","signature":"swift::constraints::ConstraintSystem::getType(swift::ASTNode) const"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
{ switch { case let c(#^COMPLETE^# b) a

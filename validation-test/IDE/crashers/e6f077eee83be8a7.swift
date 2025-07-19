// {"kind":"complete","signature":"swift::constraints::ConstraintSystem::getType(swift::ASTNode) const"}
// RUN: not --crash %target-swift-ide-test -code-completion --code-completion-token=COMPLETE -code-completion-diagnostics -source-filename %s
if case (
#a,#^COMPLETE^# is b

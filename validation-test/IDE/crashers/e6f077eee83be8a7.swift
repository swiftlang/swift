// {"kind":"complete","signature":"swift::constraints::ConstraintSystem::getType(swift::ASTNode) const","signatureAssert":"Assertion failed: (!node.isNull() && \"Expected non-null node\"), function hasType"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
if case (
#a,#^COMPLETE^# is b

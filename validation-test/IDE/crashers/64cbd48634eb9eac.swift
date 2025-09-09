// {"kind":"complete","signature":"swift::constraints::ConstraintSystem::getType(swift::ASTNode) const","signatureAssert":"Assertion failed: (hasType(node) && \"Expected type to have been set!\"), function getType"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
{ switch { case let c(#^COMPLETE^# b) a

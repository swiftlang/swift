// {"kind":"complete","original":"eb071a3e","signature":"swift::constraints::ConstraintSystem::isArgumentGenericFunction(swift::Type, swift::Expr*)","signatureAssert":"Assertion failed: (!getFixedType(tyvar)), function getUnboundBindOverloadDisjunction"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
func a() -> Sequence
a() + #^^#&

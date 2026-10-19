// {"kind":"complete","signature":"swift::constraints::Constraint::Constraint(swift::constraints::ConstraintKind, swift::Type, swift::Type, swift::constraints::ConstraintLocator*, llvm::ArrayRef<swift::TypeVariableType*>)","signatureAssert":"Assertion failed: (isAdmissibleType(Second)), function Constraint","signatureNext":"Constraint::create"}
// RUN: %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
struct a<b, c> {
  var d: a<_, _> =
    #^^#
}

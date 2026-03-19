// {"kind":"complete","original":"a09a3d40","signature":"swift::constraints::Constraint::Constraint(swift::constraints::ConstraintKind, swift::Type, swift::Type, swift::constraints::ConstraintLocator*, llvm::SmallPtrSetImpl<swift::TypeVariableType*>&)","signatureAssert":"Assertion failed: (isAdmissibleType(Second)), function Constraint","signatureNext":"Constraint::create"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
struct a {
  var b: Double
  var c = a(
    d:
      #^^#)
  func e<f, g>() -> some KeyPath<f, g>
}

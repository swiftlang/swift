// {"kind":"complete","original":"16c92173","signature":"swift::constraints::Constraint::Constraint(swift::constraints::ConstraintKind, swift::constraints::ConversionRestrictionKind, swift::Type, swift::Type, swift::constraints::ConstraintLocator*, llvm::ArrayRef<swift::TypeVariableType*>)","signatureAssert":"Assertion failed: (isAdmissibleType(second)), function Constraint","signatureNext":"Constraint::createRestricted"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
@propertyWrapper struct a<b {
  wrappedValue: b
  var projectedValue: [b]  init(projectedValue:
}
struct c {
    d<e>(@a e )
  struct f {
      g(h: [f])
    {
      d($i: h, #^^#

// {"kind":"complete","original":"2d0bf5d7","signature":"swift::constraints::ConstraintSystem::getTypeOfReferencePre(swift::constraints::OverloadChoice, swift::DeclContext*, swift::constraints::ConstraintLocatorBuilder, swift::constraints::PreparedOverloadBuilder*)","signatureAssert":"Assertion failed: (!valueType->hasUnboundGenericType() && !valueType->hasTypeParameter()), function getTypeOfReferencePre"}
// RUN: %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
{ (a: Dictionary) in
  #^^#
  let b = a {
    b
  }
  let b
}

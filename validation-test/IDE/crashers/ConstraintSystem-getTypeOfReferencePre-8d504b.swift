// {"kind":"complete","original":"ebd3c01c","signature":"swift::constraints::ConstraintSystem::getTypeOfReferencePre(swift::constraints::OverloadChoice, swift::DeclContext*, swift::constraints::ConstraintLocatorBuilder, swift::constraints::PreparedOverloadBuilder*)","signatureAssert":"Assertion failed: (!valueType->hasUnboundGenericType() && !valueType->hasTypeParameter()), function getTypeOfReferencePre"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
{
(a: Dictionary ) -> b-
  if
    guard let a
      #^^#

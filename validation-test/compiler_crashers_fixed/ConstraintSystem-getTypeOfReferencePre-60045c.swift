// {"kind":"typecheck","signature":"swift::constraints::ConstraintSystem::getTypeOfReferencePre(swift::constraints::OverloadChoice, swift::DeclContext*, swift::constraints::ConstraintLocatorBuilder, swift::constraints::PreparedOverloadBuilder*)","signatureAssert":"Assertion failed: (func->isOperator() && \"Lookup should only find operators\"), function getTypeOfReferencePre"}
// RUN: not %target-swift-frontend -typecheck %s
{
  switch if .random()  else {
  }
  {
  case let !a where a:
  }
}

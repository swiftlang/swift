// {"kind":"typecheck","original":"1b0835bd","signature":"swift::constraints::ConstraintSystem::getTypeOfMemberReferencePre(swift::constraints::OverloadChoice, swift::DeclContext*, swift::constraints::ConstraintLocator*, swift::constraints::PreparedOverloadBuilder*)","signatureAssert":"Assertion failed: (baseRValueTy->is<MetatypeType>() && \"Assumed base of unresolved member access must be a metatype\"), function getTypeOfMemberReferencePre","signatureNext":"ConstraintSystem::prepareOverloadImpl"}
// RUN: not --crash %target-swift-frontend -typecheck %s
enum a<b, AdditiveArithmetic> {
  c->b{.zero
  }

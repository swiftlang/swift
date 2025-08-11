// {"kind":"typecheck","signature":"swift::constraints::ConstraintSystem::isArgumentGenericFunction(swift::Type, swift::Expr*)","signatureAssert":"Assertion failed: (!getFixedType(tyvar)), function getUnboundBindOverloadDisjunction"}
// RUN: not --crash %target-swift-frontend -typecheck %s
{
  print($0) $00 + 0. / 1

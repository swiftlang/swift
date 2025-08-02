// {"kind":"typecheck","signature":"swift::constraints::ConstraintSystem::isArgumentGenericFunction(swift::Type, swift::Expr*)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
{
  print($0) $00 + 0. / 1

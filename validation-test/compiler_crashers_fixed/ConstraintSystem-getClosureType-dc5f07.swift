// {"kind":"typecheck","signature":"swift::constraints::ConstraintSystem::getClosureType(swift::ClosureExpr const*) const","signatureAssert":"Assertion failed: (result), function getClosureType"}
// RUN: not %target-swift-frontend -typecheck %s
{
  lazy var a =
    if <#expression#> {
      return
    }
  a
}

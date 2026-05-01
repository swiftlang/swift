// {"kind":"typecheck","original":"07aed66d","signature":"(anonymous namespace)::ConstraintGenerator::visitKeyPathExpr(swift::KeyPathExpr*)","signatureNext":"ConstraintWalker::walkToExprPost"}
// RUN: not --crash %target-swift-frontend -typecheck %s
macro
@freestanding(declaration arbitrary) macro a(for : AnyKeyPath = <#expression#>)
struct b {
  var
    c:
    String
}
#a(for : \b.c)

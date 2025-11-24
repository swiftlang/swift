// {"kind":"typecheck","signature":"swift::constraints::ConstraintSystem::getClosureType(swift::ClosureExpr const*) const","signatureAssert":"Assertion failed: (result), function getClosureType"}
// RUN: not %target-swift-frontend -typecheck %s
return {
  lazy var a = if .random() { return }
}

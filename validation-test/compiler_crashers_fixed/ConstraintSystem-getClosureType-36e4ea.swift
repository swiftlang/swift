// {"kind":"typecheck","signature":"swift::constraints::ConstraintSystem::getClosureType(swift::ClosureExpr const*) const","signatureAssert":"Assertion failed: (result), function getClosureType"}
// RUN: not %target-swift-frontend -typecheck %s
@propertyWrapper struct a<b {
  wrappedValue: b
}
func c( Bool)d {
  @a var e: Bool? =
    if .random() {
      return
    } else {
      true
    }
  c(e

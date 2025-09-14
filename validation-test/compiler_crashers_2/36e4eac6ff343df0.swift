// {"kind":"typecheck","signature":"swift::constraints::TypeVarRefCollector::walkToStmtPre(swift::Stmt*)","signatureAssert":"Assertion failed: (result), function getClosureType"}
// RUN: not --crash %target-swift-frontend -typecheck %s
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

// {"kind":"typecheck","signature":"swift::TypeChecker::typeCheckStmtConditionElement(swift::StmtConditionElement&, bool&, swift::DeclContext*)","signatureAssert":"Assertion failed: (!elt.getPattern()->hasType() && \"the pattern binding condition is already type checked\"), function typeCheckPatternBindingStmtConditionElement"}
// RUN: not %target-swift-frontend -typecheck %s
struct a {
  func b() {}
}
func foo() {
  _ = { c }
  var d: a?
  guard let d else {}
  let c = d.b()
}

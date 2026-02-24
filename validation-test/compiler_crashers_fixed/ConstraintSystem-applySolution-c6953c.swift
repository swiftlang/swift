// {"kind":"typecheck","original":"04310d73","signature":"swift::constraints::ConstraintSystem::applySolution(swift::constraints::Solution&, swift::constraints::SyntacticElementTarget)","signatureAssert":"Assertion failed: (isValidType(type) && \"type binding has invalid type\"), function applySolution"}
// RUN: not %target-swift-frontend -typecheck %s
protocol a {
  typealias b
}
protocol c: a
  struct d<e: a {
    f : e.b    let elements: e
  }
  protocol g: c
    struct h: g {
i {
  d(f: i
  elements
: self

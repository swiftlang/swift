// {"kind":"typecheck","original":"0e81571d","signature":"swift::constraints::ConstraintSystem::applySolution(swift::constraints::Solution&, swift::constraints::SyntacticElementTarget)","signatureAssert":"Assertion failed: (isValidType(type) && \"type binding has invalid type\"), function applySolution"}
// RUN: not %target-swift-frontend -typecheck %s
subscript(a: _) -> <#type#> {
  a
  0
}

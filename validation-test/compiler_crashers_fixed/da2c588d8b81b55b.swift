// {"kind":"typecheck","original":"a01bf256","signature":"swift::constraints::ConstraintSystem::applySolution(swift::constraints::Solution&, swift::constraints::SyntacticElementTarget)","signatureAssert":"Assertion failed: (isValidType(type) && \"type binding has invalid type\"), function applySolution"}
// RUN: not %target-swift-frontend -typecheck %s
enum b: Codable {
  case a(_ = encode)
}

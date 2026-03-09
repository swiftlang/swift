// {"kind":"typecheck","original":"27457093","signature":"swift::constraints::ConstraintSystem::applySolution(swift::constraints::Solution&, swift::constraints::SyntacticElementTarget)","signatureAssert":"Assertion failed: (isValidType(type) && \"type binding has invalid type\"), function applySolution"}
// RUN: not %target-swift-frontend -typecheck %s
struct a: RangeReplaceableCollection {
  var b = {
    max
    RangeReplaceableCollection
  }
}

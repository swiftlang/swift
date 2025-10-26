// {"kind":"typecheck","original":"1bb3e6fd","signature":"swift::constraints::ConstraintSystem::applySolution(swift::constraints::Solution&, swift::constraints::SyntacticElementTarget)","signatureAssert":"Assertion failed: (isValidType(type) && \"type binding has invalid type\"), function applySolution"}
// RUN: not %target-swift-frontend -typecheck %s
@propertyWrapper struct a<b {
  wrappedValue: b
}
func c(@a _ )

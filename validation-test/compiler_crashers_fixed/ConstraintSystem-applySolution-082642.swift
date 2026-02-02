// {"kind":"typecheck","signature":"swift::constraints::ConstraintSystem::applySolution(swift::constraints::Solution&, swift::constraints::SyntacticElementTarget)","signatureAssert":"Assertion failed: (isValidType(solution.simplifyType(type)) && \"node type has invalid type\"), function applySolution"}
// RUN: not %target-swift-frontend -typecheck %s
@propertyWrapper struct a<b > {
  init(wrappedValue: b)
projectedValue:
  var wrappedValue: b
}
{
  @a var c = false
}

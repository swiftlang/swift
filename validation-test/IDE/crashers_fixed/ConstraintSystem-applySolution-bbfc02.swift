// {"kind":"complete","original":"3da7ab26","signature":"swift::constraints::ConstraintSystem::applySolution(swift::constraints::Solution&, swift::constraints::SyntacticElementTarget)","signatureAssert":"Assertion failed: (isValidType(solution.simplifyType(type)) && \"node type has invalid type\"), function applySolution"}
// RUN: %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
@propertyWrapper struct a<b {
  init(wrappedValue: b)
projectedValue:
  var wrappedValue: b
}
{
  @a var c = false
  switch c {
    #^^#

// {"kind":"complete","original":"03137cdb","signature":"swift::constraints::ConstraintSystem::applySolution(swift::constraints::Solution&, swift::constraints::SyntacticElementTarget)","signatureAssert":"Assertion failed: (isValidType(type) && \"type binding has invalid type\"), function applySolution"}
// RUN: %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
@propertyWrapper struct a<b {
  wrappedValue: b
}
{
  @a var c #^^#

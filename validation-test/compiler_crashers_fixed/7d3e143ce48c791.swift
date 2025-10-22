// {"kind":"typecheck","signature":"swift::NamingPatternRequest::evaluate(swift::Evaluator&, swift::VarDecl*) const","signatureAssert":"Assertion failed: (foundVarDecl && \"VarDecl not declared in its parent?\"), function evaluate"}
// RUN: not %target-swift-frontend -typecheck %s
{
  if
  case.(let \ a) { a

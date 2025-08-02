// {"kind":"typecheck","signature":"swift::NamingPatternRequest::evaluate(swift::Evaluator&, swift::VarDecl*) const"}
// RUN: not --crash %target-swift-frontend -typecheck %s
{
  if
  case.(let \ a) { a

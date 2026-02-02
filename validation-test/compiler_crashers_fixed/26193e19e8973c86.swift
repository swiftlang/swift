// {"kind":"typecheck","original":"01146903","signature":"swift::NamingPatternRequest::evaluate(swift::Evaluator&, swift::VarDecl*) const"}
// RUN: not %target-swift-frontend -typecheck %s
switch  {
case let .a -> b b

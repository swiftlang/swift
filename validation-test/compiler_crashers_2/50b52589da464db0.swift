// {"kind":"typecheck","signature":"swift::ExprPatternMatchRequest::evaluate(swift::Evaluator&, swift::ExprPattern const*) const","signatureAssert":"Assertion failed: (EP->isResolved() && \"Must only be queried once resolved\"), function evaluate"}
// RUN: not --crash %target-swift-frontend -typecheck %s
enum a { b(c : a){{guard case.b = c

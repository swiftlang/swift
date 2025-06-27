// {"signature":"swift::ExprPatternMatchRequest::evaluate(swift::Evaluator&, swift::ExprPattern const*) const"}
// RUN: not --crash %target-swift-frontend -typecheck %s
enum a { b(c : a){{guard case.b = c

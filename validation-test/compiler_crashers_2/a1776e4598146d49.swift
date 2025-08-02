// {"kind":"typecheck","signature":"swift::PatternTypeRequest::evaluate(swift::Evaluator&, swift::ContextualPattern) const"}
// RUN: not --crash %target-swift-frontend -typecheck %s
enum a { b : _const a

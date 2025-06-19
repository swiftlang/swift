// {"signature":"swift::ParamSpecifierRequest::evaluate(swift::Evaluator&, swift::ParamDecl*) const"}
// RUN: not %target-swift-frontend -typecheck %s
func a([0])

// {"signature":"swift::CallerSideDefaultArgExprRequest::evaluate(swift::Evaluator&, swift::DefaultArgumentExpr*) const"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@expression macro a (() =
#b)
#a

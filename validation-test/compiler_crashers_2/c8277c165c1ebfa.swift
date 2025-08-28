// {"kind":"typecheck","signature":"swift::CallerSideDefaultArgExprRequest::evaluate(swift::Evaluator&, swift::DefaultArgumentExpr*) const","signatureAssert":"Assertion failed: (Loc.isValid()), function getLineAndColumnInBuffer"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@expression macro a (() =
#b)
#a

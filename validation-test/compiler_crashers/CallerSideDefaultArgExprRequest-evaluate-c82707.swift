// {"kind":"typecheck","signature":"swift::CallerSideDefaultArgExprRequest::evaluate(swift::Evaluator&, swift::DefaultArgumentExpr*) const","signatureAssert":"Assertion failed: (Loc.isValid()), function getLineAndColumnInBuffer","signatureNext":"CallerSideDefaultArgExprRequest::OutputType"}
// The crash depends on the C++ parser's handling of this input.
// UNSUPPORTED: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
@expression macro a (() =
#b)
#a

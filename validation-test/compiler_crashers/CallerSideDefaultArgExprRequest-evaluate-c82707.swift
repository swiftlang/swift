// {"kind":"typecheck","signature":"swift::CallerSideDefaultArgExprRequest::evaluate(swift::Evaluator&, swift::DefaultArgumentExpr*) const","signatureAssert":"Assertion failed: (Loc.isValid()), function getLineAndColumnInBuffer","signatureNext":"CallerSideDefaultArgExprRequest::OutputType"}
// The crash depends on the C++ parser's handling of this input, which ASTGen
// handles differently. Expect it to fail, so that we notice if it crashes again.
// XFAIL: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
@expression macro a (() =
#b)
#a

// {"kind":"typecheck","signature":"swift::CallerSideDefaultArgExprRequest::OutputType swift::Evaluator::getResultUncached<swift::CallerSideDefaultArgExprRequest, swift::CallerSideDefaultArgExprRequest::OutputType swift::evaluateOrDefault<swift::CallerSideDefaultArgExprRequest>(swift::Evaluator&, swift::CallerSideDefaultArgExprRequest, swift::CallerSideDefaultArgExprRequest::OutputType)::'lambda'()>(swift::CallerSideDefaultArgExprRequest const&, swift::CallerSideDefaultArgExprRequest::OutputType swift::evaluateOrDefault<swift::CallerSideDefaultArgExprRequest>(swift::Evaluator&, swift::CallerSideDefaultArgExprRequest, swift::CallerSideDefaultArgExprRequest::OutputType)::'lambda'())::'lambda'()::operator()() const","signatureNext":"DefaultArgumentExpr::getCallerSideDefaultExpr"}
// The crash depends on the C++ parser's handling of this input.
// UNSUPPORTED: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
func a (Int -> Int , Int =
#sourceLocation5
 func b {
  a {
    $0

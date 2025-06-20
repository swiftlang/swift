// {"signature":"swift::ClosureHasResultExprRequest::evaluate(swift::Evaluator&, swift::ClosureExpr*) const"}
// RUN: not --crash %target-swift-frontend -typecheck %s
func a (Int -> Int , Int =
#sourceLocation5
 func b {
  a {
    $0

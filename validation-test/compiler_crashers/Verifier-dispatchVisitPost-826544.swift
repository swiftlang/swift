// {"kind":"typecheck","original":"2036470e","signature":"swift::ASTWalker::PostWalkResult<swift::PackExpansionExpr*> (anonymous namespace)::Verifier::dispatchVisitPost<swift::PackExpansionExpr*>(swift::PackExpansionExpr*)","signatureNext":"Verifier::walkToExprPost"}
// RUN: not --crash %target-swift-frontend -typecheck %s
func a<each b>(c: repeat each b) {
  sequence(state: (repeat each c)) { state in
    (repeat each state)
  }
}

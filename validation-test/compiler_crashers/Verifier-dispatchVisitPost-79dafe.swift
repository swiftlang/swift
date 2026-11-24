// {"kind":"typecheck","original":"6ef3c4cc","signature":"swift::ASTWalker::PostWalkResult<swift::TupleExpr*> (anonymous namespace)::Verifier::dispatchVisitPost<swift::TupleExpr*>(swift::TupleExpr*)","signatureNext":"Verifier::walkToExprPost"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a {
  associatedtype b
}
func c<d: a>(
  e: inout d,
  f: WritableKeyPath<d, d.b>
) -> (d.b, WritableKeyPath<d, d>) {
  let g: WritableKeyPath<d, d>
  return (e[keyPath: f], g)
}

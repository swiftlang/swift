// {"signature":"swift::ASTWalker::PostWalkResult<swift::DynamicTypeExpr*> (anonymous namespace)::Verifier::dispatchVisitPost<swift::DynamicTypeExpr*>(swift::DynamicTypeExpr*)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
class a {
}
[].filter {
  type(of: $0) == a.self
}

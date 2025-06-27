// {"signature":"swift::ASTWalker::PostWalkResult<swift::MakeTemporarilyEscapableExpr*> (anonymous namespace)::Verifier::dispatchVisitPost<swift::MakeTemporarilyEscapableExpr*>(swift::MakeTemporarilyEscapableExpr*)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
func a(b: () -> Void, c: (() -> Void?) -> Void) {
   withoutActuallyEscaping(b, do : c)
 }

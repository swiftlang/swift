// {"frontendArgs":["-typecheck","-module-name","ObjectiveC","-disable-objc-attr-requires-foundation-module"],"kind":"custom","signature":"swift::ASTWalker::PostWalkResult<swift::ProtocolMetatypeToObjectExpr*> (anonymous namespace)::Verifier::dispatchVisitPost<swift::ProtocolMetatypeToObjectExpr*>(swift::ProtocolMetatypeToObjectExpr*)","signatureNext":"Verifier::walkToExprPost"}
// RUN: not --crash %target-swift-frontend -typecheck -module-name ObjectiveC -disable-objc-attr-requires-foundation-module %s
// REQUIRES: objc_interop
class Protocol {
}
@objc protocol P: ~Copyable {
}
func f() -> Protocol {
  P.self
}

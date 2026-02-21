// {"frontendArgs":["-typecheck","-enable-upcoming-feature","InferSendableFromCaptures","-strict-concurrency=minimal"],"kind":"custom","signature":"swift::AnyFunctionType::isSendable() const","signatureAssert":"Assertion failed: (!hasSendableDependentType() && \"Query Sendable dependence first\"), function isSendable"}
// RUN: not %target-swift-frontend -typecheck -enable-upcoming-feature InferSendableFromCaptures -strict-concurrency=minimal %s
// REQUIRES: swift_feature_InferSendableFromCaptures
func a()[].reduce = {
  a
}

// {"frontendArgs":["-typecheck","-enable-experimental-feature","BorrowAndMutateAccessors"],"kind":"custom","signature":"swift::StorageImplInfoRequest::evaluate(swift::Evaluator&, swift::AbstractStorageDecl*) const","signatureAssert":"Assertion failed: (Val && \"isa<> used on a null pointer\"), function doit","signatureNext":"StorageImplInfoRequest::OutputType"}
// RUN: not %target-swift-frontend -typecheck -enable-experimental-feature BorrowAndMutateAccessors %s
// REQUIRES: swift_feature_BorrowAndMutateAccessors
extension <#type#> {
  var a: <#type#> {
    borrow
  }
}

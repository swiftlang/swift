// {"extraArgs":["-enable-experimental-feature","OnceFunctions"],"kind":"emit-silgen","original":"f2ee8d19","signature":"(anonymous namespace)::Verifier::checkSourceRanges(swift::SourceRange, swift::ASTWalker::ParentTy, llvm::function_ref<void ()>)","signatureNext":"Verifier::walkToStmtPost"}
// RUN: not --crash %target-swift-frontend -emit-silgen -enable-experimental-feature OnceFunctions %s
// REQUIRES: swift_feature_OnceFunctions
var a {
  _modify {
    yield

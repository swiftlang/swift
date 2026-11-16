// {"extraArgs":["-enable-experimental-feature","Lifetimes"],"kind":"emit-silgen","signature":"swift::verificationFailure(llvm::Twine const&, swift::SILInstruction const*, llvm::function_ref<void (swift::SILPrintContext&)>)","signatureNext":"SILVerifierBase::visitBranchInst"}
// RUN: not --crash %target-swift-frontend -emit-silgen -enable-experimental-feature Lifetimes %s
// REQUIRES: swift_feature_Lifetimes
class c<g> {
}
struct d<g>: ~Escapable {
}
@_lifetime(b) func e<g>(f: Bool, a: c<g>, b: borrowing c<g>) -> d<g> {
  let i = f ? a : b
}

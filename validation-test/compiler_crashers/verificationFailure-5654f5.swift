// {"extraArgs":["-enable-sil-opaque-values"],"kind":"emit-silgen","signature":"swift::verificationFailure(llvm::Twine const&, swift::SILInstruction const*, llvm::function_ref<void (swift::SILPrintContext&)>)","signatureNext":"SILVerifierBase::visitReturnInst"}
// RUN: not --crash %target-swift-frontend -emit-silgen -enable-sil-opaque-values %s
func a<each b>(c : repeat each b) -> (repeat Optional<each b>) {
  (repeat each c)
}

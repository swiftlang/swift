// {"extraArgs":["-enable-sil-opaque-values"],"kind":"emit-silgen","signature":"swift::verificationFailure(llvm::Twine const&, swift::SILInstruction const*, llvm::function_ref<void (swift::SILPrintContext&)>)","signatureNext":"SILVerifierBase::visitTupleInst"}
// RUN: not --crash %target-swift-frontend -emit-silgen -enable-sil-opaque-values %s
struct a<b> {
  var c: ((), b)
}

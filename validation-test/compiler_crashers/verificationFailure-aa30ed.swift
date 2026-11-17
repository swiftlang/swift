// {"kind":"emit-silgen","signature":"swift::verificationFailure(llvm::Twine const&, swift::SILInstruction const*, llvm::function_ref<void (swift::SILPrintContext&)>)","signatureNext":"SILVerifierBase::visitInitExistentialRefInst"}
// RUN: not --crash %target-swift-frontend -emit-silgen %s
protocol a: AnyObject {
}
func b<c: a>(d: borrowing c) -> a {
  d
}

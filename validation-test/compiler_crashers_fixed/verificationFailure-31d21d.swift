// {"extraArgs":["-O"],"kind":"emit-sil","signature":"swift::verificationFailure(llvm::Twine const&, swift::SILInstruction const*, llvm::function_ref<void (swift::SILPrintContext&)>)","signatureNext":"SILVerifierBase::visitDebugValueInst"}
// RUN: %target-swift-frontend -emit-sil -O %s
public protocol b {
}
func c<each d>(_: repeat each d) -> Int {
  var e = 0
  return e
}
public func g(a: Int, f: b) {
  c(a, f)
}

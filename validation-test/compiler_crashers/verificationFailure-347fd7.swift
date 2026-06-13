// {"extraArgs":["-experimental-allow-module-with-compiler-errors"],"kind":"emit-sil","original":"25272672","signature":"swift::verificationFailure(llvm::Twine const&, swift::SILInstruction const*, llvm::function_ref<void (swift::SILPrintContext&)>)","signatureNext":"SILVerifierBase::visitRetainValueInst"}
// RUN: not --crash %target-swift-frontend -emit-sil -experimental-allow-module-with-compiler-errors %s
struct a: ~Copyable {
  let b: Int
}
func c() {
  let e = a(b: 0)
  let d = {
    e
  }
}

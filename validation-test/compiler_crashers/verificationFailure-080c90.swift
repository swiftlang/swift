// {"extraArgs":["-experimental-allow-module-with-compiler-errors"],"kind":"emit-sil","original":"1b7fd562","signature":"swift::verificationFailure(llvm::Twine const&, swift::SILInstruction const*, llvm::function_ref<void (swift::SILPrintContext&)>)","signatureNext":"SILVerifierBase::visitCopyAddrInst"}
// RUN: not --crash %target-swift-frontend -emit-sil -experimental-allow-module-with-compiler-errors %s
enum a<b>: ~Copyable {
  case c(b)
  func d<e>(f: e) -> a {
    switch self {
    case let error:
      error
    }
  }
}

// {"kind":"emit-silgen","signature":"swift::verificationFailure(llvm::Twine const&, swift::SILInstruction const*, llvm::function_ref<void (swift::SILPrintContext&)>)","signatureNext":"SILVerifier::checkBranchInst"}
// RUN: not --crash %target-swift-frontend -emit-silgen -cxx-interoperability-mode=default -I %S/Inputs/verificationFailure-d2bd30 %s

import FnPtr
func g(x: CInt) {
  f_ptr(x)
  f_ptr(x)
}

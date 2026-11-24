// {"kind":"emit-silgen","original":"d554473d","signature":"swift::verificationFailure(llvm::Twine const&, swift::SILInstruction const*, llvm::function_ref<void (swift::SILPrintContext&)>)","signatureNext":"SILVerifierBase::visitUpcastInst"}
// RUN: not --crash %target-swift-frontend -emit-silgen %s
class a {
  var b = 0
}
class c: a {
}
struct d<e> {
  var f: e
}
func g() {
  d { (h: borrowing c) in
    h.b
  }
}

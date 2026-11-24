// {"kind":"emit-silgen","signature":"swift::verificationFailure(llvm::Twine const&, swift::SILInstruction const*, llvm::function_ref<void (swift::SILPrintContext&)>)","signatureNext":"SILVerifierBase::visitStoreInst"}
// RUN: not --crash %target-swift-frontend -emit-silgen %s
struct a<b> {
  var c: (b, b) {
    _read {
    }
    set {
    }
  }
}
func d(_: inout Int, _: inout Int) {
}
func e(f: inout a<Int>) {
  d(&f.c.0, &f.c.1)
}

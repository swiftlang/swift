// {"kind":"emit-silgen","signature":"swift::verificationFailure(llvm::Twine const&, swift::SILInstruction const*, llvm::function_ref<void (swift::SILPrintContext&)>)","signatureNext":"SILVerifierBase::visitApplyInst"}
// RUN: not --crash %target-swift-frontend -emit-silgen %s
@propertyWrapper
struct W {
  var wrappedValue: Int {
    0
  }
  init(wrappedValue: Int) async {
  }
}
struct S {
  @W var x: Int
}
_ = S(x: 0)

// {"frontendArgs":["-emit-silgen","-parse-stdlib","-language-mode","6"],"kind":"custom","signature":"swift::verificationFailure(llvm::Twine const&, swift::SILInstruction const*, llvm::function_ref<void (swift::SILPrintContext&)>)","signatureNext":"verifyFlowSensitiveRules"}
// RUN: not --crash %target-swift-frontend -emit-silgen -parse-stdlib -language-mode 6 %s
func f(_ w: Builtin.Word) {
  let p = Builtin.stackAlloc(w, w, w)
  defer {
    Builtin.stackDealloc(p)
  }
}

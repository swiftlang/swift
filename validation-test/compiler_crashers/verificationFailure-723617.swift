// {"kind":"emit-silgen","original":"bd94598d","signature":"swift::verificationFailure(llvm::Twine const&, swift::SILFunction const*, llvm::function_ref<void (swift::SILPrintContext&)>)","signatureNext":"SILFunction::verify"}
// RUN: not --crash %target-swift-frontend -emit-silgen %s
actor a {
  func b() {
    [
      { [unowned self] in
        b
      }
    ]
  }
}

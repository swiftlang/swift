// {"kind":"emit-silgen","original":"c2ba4640","signature":"(anonymous namespace)::CallEmission::emitArgumentsForNormalApply(swift::Lowering::AbstractionPattern, swift::CanTypeWrapper<swift::SILFunctionType>, llvm::ArrayRef<swift::LifetimeDependenceInfo>, swift::ForeignInfo const&, llvm::SmallVectorImpl<swift::Lowering::ManagedValue>&, std::__1::optional<swift::SILLocation>&)","signatureAssert":"Assertion failed: (fnConv.getSILType(Params[i + firstCapture], typeExpansionContext) == captures[i].getType() && \"capture doesn't match param type\"), function claimCaptureParams","signatureNext":"CallEmission::apply"}
// RUN: not --crash %target-swift-frontend -emit-silgen %s
func a<b>(c: borrowing b) {
  {
    c
  }()
}

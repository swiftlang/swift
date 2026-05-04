// {"kind":"emit-silgen","original":"10381b0d","signature":"emitRawApply(swift::Lowering::SILGenFunction&, swift::SILLocation, swift::Lowering::ManagedValue, swift::SubstitutionMap, llvm::ArrayRef<swift::Lowering::ManagedValue>, swift::CanTypeWrapper<swift::SILFunctionType>, swift::optionset::OptionSet<swift::ApplyFlags, unsigned char>, llvm::ArrayRef<swift::SILValue>, swift::SILValue, llvm::SmallVectorImpl<swift::SILValue>&, swift::Lowering::ExecutorBreadcrumb)","signatureAssert":"Assertion failed: (indResultTy == indResultAddr->getType()), function emitRawApply","signatureNext":"Lowering::SILGenFunction::emitApply"}
// RUN: not --crash %target-swift-frontend -emit-silgen %s
// REQUIRES: OS=macosx
protocol a<b> {
  associatedtype b: d where b.e == Self
  associatedtype f
  func n() -> f?
}
protocol d {
  associatedtype e: a
}
func g<h, i>(
  j: h, k: i
) {
}
func l<m: d>(
  o: [a<m>]
) {
  o.reduce(0) { cc, c in
    c.n == nil ? 0 : 1
  }
}

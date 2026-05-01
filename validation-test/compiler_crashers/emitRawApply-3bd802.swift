// {"kind":"emit-silgen","original":"60b1fd7d","signature":"emitRawApply(swift::Lowering::SILGenFunction&, swift::SILLocation, swift::Lowering::ManagedValue, swift::SubstitutionMap, llvm::ArrayRef<swift::Lowering::ManagedValue>, swift::CanTypeWrapper<swift::SILFunctionType>, swift::optionset::OptionSet<swift::ApplyFlags, unsigned char>, llvm::ArrayRef<swift::SILValue>, swift::SILValue, llvm::SmallVectorImpl<swift::SILValue>&, swift::Lowering::ExecutorBreadcrumb)","signatureNext":"Lowering::SILGenFunction::emitApply"}
// RUN: not --crash %target-swift-frontend -emit-silgen %s
func a(@_inheritActorContext b: @escaping () async -> Void) {
}
actor c {
  func d() {
    a {
      self
    }
  }
}

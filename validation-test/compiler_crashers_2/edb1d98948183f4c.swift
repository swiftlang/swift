// {"kind":"emit-silgen","signature":"emitRawApply(swift::Lowering::SILGenFunction&, swift::SILLocation, swift::Lowering::ManagedValue, swift::SubstitutionMap, llvm::ArrayRef<swift::Lowering::ManagedValue>, swift::CanTypeWrapper<swift::SILFunctionType>, swift::optionset::OptionSet<swift::ApplyFlags, unsigned char>, llvm::ArrayRef<swift::SILValue>, swift::SILValue, llvm::SmallVectorImpl<swift::SILValue>&, swift::Lowering::ExecutorBreadcrumb)"}
// RUN: not --crash %target-swift-frontend -emit-silgen %s
protocol a {
  func b() -> [(_) -> Self]
}
do {
  func c(d : a) {
    let e = d.b
  }
}

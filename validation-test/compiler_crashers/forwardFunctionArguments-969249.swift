// {"kind":"emit-silgen","signature":"forwardFunctionArguments(swift::Lowering::SILGenFunction&, swift::SILLocation, swift::CanTypeWrapper<swift::SILFunctionType>, llvm::ArrayRef<swift::Lowering::ManagedValue>, llvm::SmallVectorImpl<swift::SILValue>&, swift::optionset::OptionSet<swift::Lowering::SILGenFunction::ThunkGenFlag, unsigned int>)","signatureAssert":"Assertion failed: (Index < Length && \"Invalid index!\"), function operator[]","signatureNext":"Lowering::SILGenFunction::createWithoutActuallyEscapingClosure"}
// RUN: not --crash %target-swift-frontend -emit-silgen %s
func a<b>(c: @isolated(any) () -> b) {
  withoutActuallyEscaping(c) { d in
  }
}

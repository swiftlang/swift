// {"kind":"emit-silgen","signature":"swift::ApplyInstBase<swift::PartialApplyInst, swift::SingleValueInstruction, false>::ApplyInstBase<swift::SILType>(swift::SILInstructionKind, swift::SILDebugLocation, swift::SILValue, swift::SILType, swift::SubstitutionMap, llvm::ArrayRef<swift::SILValue>, llvm::ArrayRef<swift::SILValue>, std::__1::optional<llvm::ArrayRef<swift::SILLocation>>, swift::GenericSpecializationInformation const*, swift::SILType)","signatureAssert":"Assertion failed: (!!subs == !!callee->getType().castTo<SILFunctionType>() ->getInvocationGenericSignature()), function ApplyInstBase","signatureNext":"PartialApplyInst::create"}
// RUN: not --crash %target-swift-frontend -emit-silgen %s
@propertyWrapper
struct a<b> {
  var wrappedValue: b
}
func c<d>() -> d {
  @a var e: Int
  e = 2
}

// {"kind":"emit-silgen","original":"5be7fe62","signature":"swift::buildSILFunctionThunkType(swift::SILFunction*, swift::CanTypeWrapper<swift::SILFunctionType>&, swift::CanTypeWrapper<swift::SILFunctionType>&, swift::CanType&, swift::CanType&, swift::GenericEnvironment*&, swift::SubstitutionMap&, swift::CanType&, bool, std::__1::optional<swift::DifferentiationThunkKind>)","signatureAssert":"Assertion failed: (hasDynamicSelfMetadata() && \"This method can only be called if the \" \"SILFunction has a self-metadata parameter\"), function getDynamicSelfMetadata","signatureNext":"createThunk"}
// RUN: not --crash %target-swift-frontend -emit-silgen %s
protocol a {
  func b(c: (Self) -> Void) -> Self
}
class d: a {
  func b(c: (Self) -> Void) -> Self {
  }
}

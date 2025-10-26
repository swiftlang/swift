// {"kind":"emit-silgen","original":"6943e75f","signature":"swift::SILFunctionType::SILFunctionType(swift::GenericSignature, swift::SILExtInfo, swift::SILCoroutineKind, swift::ParameterConvention, llvm::ArrayRef<swift::SILParameterInfo>, llvm::ArrayRef<swift::SILYieldInfo>, llvm::ArrayRef<swift::SILResultInfo>, std::__1::optional<swift::SILResultInfo>, swift::SubstitutionMap, swift::SubstitutionMap, swift::ASTContext const&, swift::RecursiveTypeProperties, swift::ProtocolConformanceRef)"}
// RUN: not %target-swift-frontend -emit-silgen %s
func a<each b : BinaryInteger>(c : repeat each b) {
  UInt32(repeat each c)
}

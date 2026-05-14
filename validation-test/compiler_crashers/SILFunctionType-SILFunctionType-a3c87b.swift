// {"kind":"emit-silgen","original":"efe0eec7","signature":"swift::SILFunctionType::SILFunctionType(swift::GenericSignature, swift::SILExtInfo, swift::SILCoroutineKind, swift::ParameterConvention, llvm::ArrayRef<swift::SILParameterInfo>, llvm::ArrayRef<swift::SILYieldInfo>, llvm::ArrayRef<swift::SILResultInfo>, std::__1::optional<swift::SILResultInfo>, swift::SubstitutionMap, swift::SubstitutionMap, swift::ASTContext const&, swift::RecursiveTypeProperties, swift::ProtocolConformanceRef)","signatureAssert":"Assertion failed: (!hasIsolatedParameter && \"building SIL function type with multiple isolated parameters\"), function SILFunctionType","signatureNext":"SILFunctionType::get"}
// RUN: not --crash %target-swift-frontend -emit-silgen %s
actor a {
}
actor d {
}
func b(c: isolated a, e: isolated d) {
}

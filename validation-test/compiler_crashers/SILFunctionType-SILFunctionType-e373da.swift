// {"kind":"emit-silgen","original":"f1c7c406","signature":"swift::SILFunctionType::SILFunctionType(swift::GenericSignature, swift::SILExtInfo, swift::SILCoroutineKind, swift::ParameterConvention, llvm::ArrayRef<swift::SILParameterInfo>, llvm::ArrayRef<swift::SILYieldInfo>, llvm::ArrayRef<swift::SILResultInfo>, std::__1::optional<swift::SILResultInfo>, swift::SubstitutionMap, swift::SubstitutionMap, swift::ASTContext const&, swift::RecursiveTypeProperties, swift::ProtocolConformanceRef)","signatureAssert":"Assertion failed: (!isa<PackExpansionType>(yield.getInterfaceType()) && \"Cannot have a pack expansion directly as a yield\"), function SILFunctionType","signatureNext":"SILFunctionType::get"}
// RUN: not --crash %target-swift-frontend -emit-silgen %s
@available(SwiftStdlib 5.9, *)
struct a<each b> {
  var c: (repeat each b) {
    _read {
    }
  }
}

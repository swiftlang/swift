// {"kind":"emit-silgen","original":"dc21821b","signature":"getSILFunctionType(swift::Lowering::TypeConverter&, swift::TypeExpansionContext, swift::Lowering::AbstractionPattern, swift::CanTypeWrapper<swift::AnyFunctionType>, swift::SILExtInfoBuilder, (anonymous namespace)::Conventions const&, swift::ForeignInfo const&, std::__1::optional<swift::SILDeclRef>, std::__1::optional<swift::SILDeclRef>, std::__1::optional<swift::SubstitutionMap>, swift::ProtocolConformanceRef)","signatureAssert":"Assertion failed: (!cast<ParamDecl>(varDecl)->supportsMutation() && \"Cannot capture a pack as an lvalue\"), function lowerCaptureContextParameters","signatureNext":"getNativeSILFunctionType"}
// RUN: not --crash %target-swift-frontend -emit-silgen %s
func a<each b>(c: repeat sending each b) {
  Task {
    repeat each c
  }
}

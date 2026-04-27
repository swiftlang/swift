// {"extraArgs":["-experimental-allow-module-with-compiler-errors","-cxx-interoperability-mode=default","-emit-clang-header-min-access","internal","-emit-clang-header-path","/dev/null"],"kind":"typecheck","original":"0439ba2e","signature":"swift::Lowering::AbstractionPattern::initSwiftType(swift::SubstitutionMap, swift::CanGenericSignature, swift::CanType, swift::Lowering::AbstractionPattern::Kind)","signatureAssert":"Assertion failed: (OrigType == signature.getReducedType(origType)), function initSwiftType","signatureNext":"LowerType::visitAnyStructType"}
// RUN: not --crash %target-swift-frontend -typecheck -experimental-allow-module-with-compiler-errors -cxx-interoperability-mode=default -emit-clang-header-min-access internal -emit-clang-header-path /dev/null %s
protocol a {
  associatedtype b
}
@propertyWrapper struct f<each c> {
  var wrappedValue: (repeat each c)
}
struct d<each e: a> {
  @f var g: (repeat each e.b)
}

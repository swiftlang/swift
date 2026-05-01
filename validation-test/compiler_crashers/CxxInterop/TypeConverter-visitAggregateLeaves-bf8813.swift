// {"extraArgs":["-experimental-allow-module-with-compiler-errors","-cxx-interoperability-mode=default","-emit-clang-header-min-access","internal","-emit-clang-header-path","/dev/null"],"kind":"emit-sil","original":"08c7e142","signature":"swift::Lowering::TypeConverter::visitAggregateLeaves(swift::Lowering::AbstractionPattern, swift::CanType, swift::TypeExpansionContext, std::__1::function<bool (swift::CanType, swift::Lowering::AbstractionPattern, swift::ValueDecl*, std::__1::optional<unsigned int>)>, std::__1::function<bool (swift::CanType, swift::Lowering::AbstractionPattern, swift::ValueDecl*, std::__1::optional<unsigned int>)>)","signatureAssert":"Assertion failed: (false), function operator()","signatureNext":"Lowering::TypeConverter::verifyTrivialLowering"}
// RUN: not --crash %target-swift-frontend -emit-sil -experimental-allow-module-with-compiler-errors -cxx-interoperability-mode=default -emit-clang-header-min-access internal -emit-clang-header-path /dev/null %s
enum a {
  case (() -> Bool)
  case (a)
}

// {"extraArgs":["-experimental-allow-module-with-compiler-errors","-cxx-interoperability-mode=default","-emit-clang-header-min-access","internal","-emit-clang-header-path","/dev/null"],"kind":"emit-sil","original":"034478b5","signature":"swift::Lowering::TypeConverter::verifyTrivialLowering(swift::Lowering::TypeLowering const&, swift::Lowering::AbstractionPattern, swift::CanType, swift::TypeExpansionContext)","signatureAssert":"Assertion failed: (false), function verifyTrivialLowering","signatureNext":"Lowering::TypeConverter::getTypeLowering"}
// RUN: not --crash %target-swift-frontend -emit-sil -experimental-allow-module-with-compiler-errors -cxx-interoperability-mode=default -emit-clang-header-min-access internal -emit-clang-header-path /dev/null %s
struct a {
  var b: a
}

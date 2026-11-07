// {"extraArgs":["-experimental-allow-module-with-compiler-errors","-cxx-interoperability-mode=default","-emit-clang-header-min-access","internal","-emit-clang-header-path","/dev/null"],"kind":"typecheck","original":"1f5b8a97","signature":"swift::Lowering::AbstractionPattern::initSwiftType(swift::SubstitutionMap, swift::CanGenericSignature, swift::CanType, swift::Lowering::AbstractionPattern::Kind)","signatureAssert":"Assertion failed: (signature || !origType->hasTypeParameter()), function initSwiftType","signatureNext":"Lowering::AbstractionPattern::getSubstFunctionTypePattern"}
// The crash depends on the C++ parser's handling of this input, which ASTGen
// handles differently. Expect it to fail, so that we notice if it crashes again.
// XFAIL: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck -experimental-allow-module-with-compiler-errors -cxx-interoperability-mode=default -emit-clang-header-min-access internal -emit-clang-header-path /dev/null %s
struct a<b {
  c: b
  @globalActor actor d {
    static  shared =
  }
  e->
  @d()
  ->Void

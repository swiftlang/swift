// {"extraArgs":["-experimental-allow-module-with-compiler-errors","-cxx-interoperability-mode=default","-emit-clang-header-min-access","internal","-emit-clang-header-path","/dev/null"],"kind":"typecheck","original":"0eca28e8","signature":"swift::PackElementType::PackElementType(swift::Type, unsigned int, swift::RecursiveTypeProperties, swift::ASTContext const*)","signatureAssert":"Assertion failed: (packType->isParameterPack() || packType->is<PackArchetypeType>() || packType->isTypeVariableOrMember()), function PackElementType","signatureNext":"PackElementType::get"}
// RUN: not --crash %target-swift-frontend -typecheck -experimental-allow-module-with-compiler-errors -cxx-interoperability-mode=default -emit-clang-header-min-access internal -emit-clang-header-path /dev/null %s
struct a<each b, each c> {
  typealias d<e, h> = (e) -> Void
  typealias f<e> = (repeat d<e, each c>)
  var g: (repeat f<each b>)
}

// {"extraArgs":["-experimental-allow-module-with-compiler-errors","-cxx-interoperability-mode=default","-emit-clang-header-min-access","internal","-emit-clang-header-path","/dev/null"],"kind":"typecheck","original":"f73b4c7f","signature":"swift::Lowering::TypeConverter::getTypeLowering(swift::Lowering::AbstractionPattern, swift::Type, swift::TypeExpansionContext)","signatureAssert":"Assertion failed: (hasNoNontrivialLexicalLeaf && \"Found non-trivial lexical leaf in non-trivial non-lexical type?!\"), function verifyLexicalLowering","signatureNext":"SILType::getFieldType"}
// RUN: not --crash %target-swift-frontend -typecheck -experimental-allow-module-with-compiler-errors -cxx-interoperability-mode=default -emit-clang-header-min-access internal -emit-clang-header-path /dev/null %s
struct a {
  struct b<c> {
    var d: c
    var e: some Collection = []
  }
  var f = b(d: 0)
}

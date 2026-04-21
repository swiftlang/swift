// {"extraArgs":["-experimental-allow-module-with-compiler-errors","-cxx-interoperability-mode=default","-emit-clang-header-min-access","internal","-emit-clang-header-path","/dev/null"],"kind":"emit-sil","original":"353faf22","signature":"swift::Lowering::TypeConverter::getTypeLowering(swift::Lowering::AbstractionPattern, swift::Type, swift::TypeExpansionContext)","signatureAssert":"Assertion failed: (hasNoNontrivialLexicalLeaf && \"Found non-trivial lexical leaf in non-trivial non-lexical type?!\"), function verifyLexicalLowering","signatureNext":"SILTypeSubstituter::visitType"}
// RUN: not --crash %target-swift-frontend -emit-sil -experimental-allow-module-with-compiler-errors -cxx-interoperability-mode=default -emit-clang-header-min-access internal -emit-clang-header-path /dev/null %s
protocol a
  struct b: a {
    c: some Equatable = ""
    func d -> some a {
      b(

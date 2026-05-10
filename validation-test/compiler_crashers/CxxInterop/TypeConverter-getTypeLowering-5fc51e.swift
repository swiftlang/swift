// {"extraArgs":["-experimental-allow-module-with-compiler-errors","-cxx-interoperability-mode=default","-emit-clang-header-min-access","internal","-emit-clang-header-path","/dev/null"],"kind":"typecheck","original":"a15b7668","signature":"swift::Lowering::TypeConverter::getTypeLowering(swift::Lowering::AbstractionPattern, swift::Type, swift::TypeExpansionContext)","signatureAssert":"Assertion failed: (hasNoNontrivialLexicalLeaf && \"Found non-trivial lexical leaf in non-trivial non-lexical type?!\"), function verifyLexicalLowering","signatureNext":"Lowering::TypeConverter::visitAggregateLeaves"}
// RUN: not --crash %target-swift-frontend -typecheck -experimental-allow-module-with-compiler-errors -cxx-interoperability-mode=default -emit-clang-header-min-access internal -emit-clang-header-path /dev/null %s
protocol a {
  associatedtype b
}
struct c< d : a > {
  enum e {
    case (d.b)
  }
  var
    f: e
}
struct g< d : a where d.b == String > {
  var
    h : c< d >
}

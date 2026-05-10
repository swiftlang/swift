// {"extraArgs":["-experimental-allow-module-with-compiler-errors","-cxx-interoperability-mode=default","-emit-clang-header-min-access","internal","-emit-clang-header-path","/dev/null"],"kind":"emit-sil","original":"90acd51c","signature":"(anonymous namespace)::LowerType::visitAnyStructType(swift::CanType, swift::Lowering::AbstractionPattern, swift::StructDecl*, swift::IsTypeExpansionSensitive_t)","stackOverflow":true}
// RUN: not --crash %target-swift-frontend -emit-sil -experimental-allow-module-with-compiler-errors -cxx-interoperability-mode=default -emit-clang-header-min-access internal -emit-clang-header-path /dev/null %s
struct a<b {
  enum c
    let d: a<c>
    e: a<Never>

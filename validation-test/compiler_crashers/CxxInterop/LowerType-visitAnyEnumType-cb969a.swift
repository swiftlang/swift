// {"extraArgs":["-experimental-allow-module-with-compiler-errors","-cxx-interoperability-mode=default","-emit-clang-header-min-access","internal","-emit-clang-header-path","/dev/null"],"kind":"typecheck","original":"d93c5056","signature":"(anonymous namespace)::LowerType::visitAnyEnumType(swift::CanType, swift::Lowering::AbstractionPattern, swift::EnumDecl*, swift::IsTypeExpansionSensitive_t)","stackOverflow":true}
// RUN: not --crash %target-swift-frontend -typecheck -experimental-allow-module-with-compiler-errors -cxx-interoperability-mode=default -emit-clang-header-min-access internal -emit-clang-header-path /dev/null %s
enum a<b> {
  case (a<a>)
}

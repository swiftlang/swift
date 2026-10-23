// {"extraArgs":["-experimental-allow-module-with-compiler-errors","-cxx-interoperability-mode=default","-emit-clang-header-min-access","internal","-emit-clang-header-path","/dev/null"],"kind":"typecheck","original":"34711868","signature":"(anonymous namespace)::TypeClassifierBase<(anonymous namespace)::LowerType, swift::Lowering::TypeLowering*>::visit(swift::CanType, swift::Lowering::AbstractionPattern, swift::IsTypeExpansionSensitive_t)","stackOverflow":true}
// RUN: not --crash %target-swift-frontend -typecheck -experimental-allow-module-with-compiler-errors -cxx-interoperability-mode=default -emit-clang-header-min-access internal -emit-clang-header-path /dev/null %s
struct a<b {
  class c
    let d: a<(Substring, c)>
    let e: a<()>

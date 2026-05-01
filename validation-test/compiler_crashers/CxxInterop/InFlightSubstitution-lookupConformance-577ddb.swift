// {"extraArgs":["-experimental-allow-module-with-compiler-errors","-cxx-interoperability-mode=default","-emit-clang-header-min-access","internal","-emit-clang-header-path","/dev/null"],"kind":"typecheck","original":"001bd4a5","signature":"swift::InFlightSubstitution::lookupConformance(swift::Type, swift::ProtocolDecl*, unsigned int)","stackOverflow":true}
// RUN: not --crash %target-swift-frontend -typecheck -experimental-allow-module-with-compiler-errors -cxx-interoperability-mode=default -emit-clang-header-min-access internal -emit-clang-header-path /dev/null %s
enum a<b: Hashable> {
  case (a<[b]>)
}

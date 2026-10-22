// {"extraArgs":["-experimental-allow-module-with-compiler-errors","-cxx-interoperability-mode=default","-emit-clang-header-min-access","internal","-emit-clang-header-path","/dev/null"],"kind":"typecheck","original":"a1296288","signature":"swift::TypeTransform<(anonymous namespace)::TypeSubstituter>::doIt(swift::Type, swift::TypePosition)","signatureNext":"Type::subst","stackOverflow":true}
// RUN: not --crash %target-swift-frontend -typecheck -experimental-allow-module-with-compiler-errors -cxx-interoperability-mode=default -emit-clang-header-min-access internal -emit-clang-header-path /dev/null %s
enum a<b> {
  case (b, [a<Self>])
}

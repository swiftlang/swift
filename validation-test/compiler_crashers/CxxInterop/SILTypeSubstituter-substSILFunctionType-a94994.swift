// {"extraArgs":["-experimental-allow-module-with-compiler-errors","-cxx-interoperability-mode=default","-emit-clang-header-min-access","internal","-emit-clang-header-path","/dev/null"],"kind":"typecheck","original":"2d964991","signature":"(anonymous namespace)::SILTypeSubstituter::substSILFunctionType(swift::CanTypeWrapper<swift::SILFunctionType>, bool)","signatureNext":"SILFunctionType::substituteOpaqueArchetypes"}
// RUN: not --crash %target-swift-frontend -typecheck -experimental-allow-module-with-compiler-errors -cxx-interoperability-mode=default -emit-clang-header-min-access internal -emit-clang-header-path /dev/null %s
protocol a
  func b<c: a>(c ) -> some a {
    b(d(
  }
  func d -> some a {
    b(d(

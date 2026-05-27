// {"extraArgs":["-experimental-allow-module-with-compiler-errors","-cxx-interoperability-mode=default","-emit-clang-header-min-access","internal","-emit-clang-header-path","/dev/null"],"kind":"typecheck","original":"0d57ef17","signature":"swift::Lowering::TypeConverter::getDeclCaptureKind(swift::CapturedValue, swift::TypeExpansionContext)","signatureAssert":"Assertion failed: (contextTy->isCopyable() && \"Not implemented\"), function getDeclCaptureKind","signatureNext":"getSILFunctionType"}
// RUN: not --crash %target-swift-frontend -typecheck -experimental-allow-module-with-compiler-errors -cxx-interoperability-mode=default -emit-clang-header-min-access internal -emit-clang-header-path /dev/null %s
guard
  func a {
    each b

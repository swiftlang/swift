// {"extraArgs":["-experimental-allow-module-with-compiler-errors","-cxx-interoperability-mode=default","-emit-clang-header-min-access","internal","-emit-clang-header-path","/dev/null"],"kind":"emit-sil","original":"9188dc09","signature":"swift::irgen::TypeConverter::convertStructType(swift::TypeBase*, swift::CanType, swift::StructDecl*)","signatureNext":"TypeConverter::convertAnyNominalType"}
// RUN: not --crash %target-swift-frontend -emit-sil -experimental-allow-module-with-compiler-errors -cxx-interoperability-mode=default -emit-clang-header-min-access internal -emit-clang-header-path /dev/null %s
protocol a
  struct b: a {
    c: some a = b(
    let d =

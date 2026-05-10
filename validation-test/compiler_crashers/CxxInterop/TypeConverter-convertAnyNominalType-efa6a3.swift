// {"extraArgs":["-experimental-allow-module-with-compiler-errors","-cxx-interoperability-mode=default","-emit-clang-header-min-access","internal","-emit-clang-header-path","/dev/null"],"kind":"typecheck","original":"dac3e0c2","signature":"swift::irgen::TypeConverter::convertAnyNominalType(swift::CanType, swift::NominalTypeDecl*)","signatureAssert":"Assertion failed: (decl->getDeclaredType()->hasUnboundGenericType()), function convertAnyNominalType","signatureNext":"TypeConverter::convertType"}
// RUN: not --crash %target-swift-frontend -typecheck -experimental-allow-module-with-compiler-errors -cxx-interoperability-mode=default -emit-clang-header-min-access internal -emit-clang-header-path /dev/null %s
protocol a
  extension a {
    struct b
    }
    struct c: a {  d: b

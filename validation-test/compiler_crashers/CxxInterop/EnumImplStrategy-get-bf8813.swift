// {"extraArgs":["-experimental-allow-module-with-compiler-errors","-cxx-interoperability-mode=default","-emit-clang-header-min-access","internal","-emit-clang-header-path","/dev/null"],"kind":"emit-sil","original":"08c7e142","signature":"swift::irgen::EnumImplStrategy::get(swift::irgen::TypeConverter&, swift::SILType, swift::EnumDecl*)","signatureNext":"TypeConverter::convertEnumType"}
// RUN: not --crash %target-swift-frontend -emit-sil -experimental-allow-module-with-compiler-errors -cxx-interoperability-mode=default -emit-clang-header-min-access internal -emit-clang-header-path /dev/null %s
enum a {
  case (() -> Bool)
  case (a)
}

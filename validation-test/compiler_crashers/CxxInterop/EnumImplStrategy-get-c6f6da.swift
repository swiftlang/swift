// {"extraArgs":["-experimental-allow-module-with-compiler-errors","-cxx-interoperability-mode=default","-emit-clang-header-path","/dev/null"],"kind":"typecheck","original":"450fc175","signature":"swift::irgen::EnumImplStrategy::get(swift::irgen::TypeConverter&, swift::SILType, swift::EnumDecl*)","signatureAssert":"Assertion failed: (elementsWithPayload.empty() && \"C enum with payload?!\"), function get","signatureNext":"TypeConverter::convertEnumType"}
// RUN: not --crash %target-swift-frontend -typecheck -experimental-allow-module-with-compiler-errors -cxx-interoperability-mode=default -emit-clang-header-path /dev/null %s
@c public enum a: Int {
  case b
  indirect case ()
}

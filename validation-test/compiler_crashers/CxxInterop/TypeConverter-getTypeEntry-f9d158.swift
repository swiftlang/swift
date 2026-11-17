// {"extraArgs":["-experimental-allow-module-with-compiler-errors","-cxx-interoperability-mode=default","-emit-clang-header-min-access","internal","-emit-clang-header-path"],"kind":"typecheck","original":"72eb105d","signature":"swift::irgen::TypeConverter::getTypeEntry(swift::CanType)","signatureNext":"IRGenModule::getTypeInfo","splits":[0]}
// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: not --crash %target-swift-frontend -typecheck -experimental-allow-module-with-compiler-errors -cxx-interoperability-mode=default -emit-clang-header-min-access internal -emit-clang-header-path %t/main.swift %t/x1.swift
//--- main.swift

//--- x1.swift
enum a<b, c> {
  case (config: a<Int, String>)
}

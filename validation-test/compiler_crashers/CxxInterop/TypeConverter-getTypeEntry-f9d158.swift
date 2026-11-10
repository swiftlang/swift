// {"extraArgs":["-experimental-allow-module-with-compiler-errors","-cxx-interoperability-mode=default","-emit-clang-header-min-access","internal","-emit-clang-header-path"],"kind":"typecheck","original":"72eb105d","signature":"swift::irgen::TypeConverter::getTypeEntry(swift::CanType)","signatureNext":"IRGenModule::getTypeInfo","splits":[0]}
// The crash depends on the C++ parser's handling of this input, which ASTGen
// handles differently. Expect it to fail, so that we notice if it crashes again.
// XFAIL: parser_astgen
// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: not --crash %target-swift-frontend -typecheck -experimental-allow-module-with-compiler-errors -cxx-interoperability-mode=default -emit-clang-header-min-access internal -emit-clang-header-path %t/main.swift %t/x1.swift
//--- main.swift

//--- x1.swift
enum a<b, c> {
  case (config: a<Int, String>)
}

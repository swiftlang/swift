// {"extraArgs":["-experimental-allow-module-with-compiler-errors","-emit-clang-header-path","/dev/null"],"kind":"typecheck","original":"3929f0c0","signature":"swift::DeclAndTypePrinter::Implementation::visitEnumDecl(swift::EnumDecl*)","signatureAssert":"Assertion failed: (Val && \"isa<> used on a null pointer\"), function doit","signatureNext":"DeclAndTypePrinter::print"}
// RUN: not --crash %target-swift-frontend -typecheck -experimental-allow-module-with-compiler-errors -emit-clang-header-path /dev/null %s
// REQUIRES: objc_interop
@objc enum a: Int {
  case ()
}

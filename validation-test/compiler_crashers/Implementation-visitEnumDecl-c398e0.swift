// {"extraArgs":["-experimental-allow-module-with-compiler-errors","-emit-clang-header-path","/dev/null"],"kind":"typecheck","original":"9a678469","signature":"swift::DeclAndTypePrinter::Implementation::visitEnumDecl(swift::EnumDecl*)","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast","signatureNext":"DeclAndTypePrinter::print"}
// RUN: not --crash %target-swift-frontend -typecheck -experimental-allow-module-with-compiler-errors -emit-clang-header-path /dev/null %s
// REQUIRES: objc_interop
@objc enum a: Int {
  case  = 0.0.0
}

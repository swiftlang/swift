// {"extraArgs":["-experimental-allow-module-with-compiler-errors","-cxx-interoperability-mode=default","-emit-clang-header-min-access","internal","-emit-clang-header-path","/dev/null"],"kind":"typecheck","original":"00e0f063","signature":"swift::DeclAndTypePrinter::Implementation::visitEnumDeclCxx(swift::EnumDecl*)::'lambda'(swift::EnumElementDecl*)::operator()(swift::EnumElementDecl*) const","signatureAssert":"Assertion failed: (!empty()), function front","signatureNext":"ClangValueTypePrinter::printValueTypeDecl"}
// RUN: not --crash %target-swift-frontend -typecheck -experimental-allow-module-with-compiler-errors -cxx-interoperability-mode=default -emit-clang-header-min-access internal -emit-clang-header-path /dev/null %s
enum a {
  case b()
  case c
}

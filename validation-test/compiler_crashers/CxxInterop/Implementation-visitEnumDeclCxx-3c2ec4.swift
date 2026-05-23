// {"extraArgs":["-experimental-allow-module-with-compiler-errors","-cxx-interoperability-mode=default","-emit-clang-header-min-access","internal","-emit-clang-header-path","/dev/null"],"kind":"typecheck","original":"5b0f7ed4","signature":"swift::DeclAndTypePrinter::Implementation::visitEnumDeclCxx(swift::EnumDecl*)::'lambda'(llvm::StringRef, swift::EnumElementDecl*, std::__1::optional<swift::IRABIDetailsProvider::EnumElementInfo>)::operator()(llvm::StringRef, swift::EnumElementDecl*, std::__1::optional<swift::IRABIDetailsProvider::EnumElementInfo>) const","signatureAssert":"Assertion failed: (!support.isUnsupported()), function printCustomCxxFunction","signatureNext":"ClangValueTypePrinter::printValueTypeDecl"}
// RUN: not --crash %target-swift-frontend -typecheck -experimental-allow-module-with-compiler-errors -cxx-interoperability-mode=default -emit-clang-header-min-access internal -emit-clang-header-path /dev/null %s
enum a {
  case b(a.Type?)
}

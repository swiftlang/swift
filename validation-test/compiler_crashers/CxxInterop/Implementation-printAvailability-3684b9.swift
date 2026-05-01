// {"extraArgs":["-experimental-allow-module-with-compiler-errors","-cxx-interoperability-mode=default","-emit-clang-header-min-access","internal","-emit-clang-header-path","/dev/null"],"kind":"typecheck","original":"2e3e8371","signature":"swift::DeclAndTypePrinter::Implementation::printAvailability(llvm::raw_ostream&, swift::Decl const*, swift::DeclAndTypePrinter::Implementation::PrintLeadingSpace)","signatureAssert":"Assertion failed: (shouldInclude(renamedDecl) && \"ObjC printer logic mismatch with renamed decl\"), function printRenameForDecl","signatureNext":"DeclAndTypePrinter::printAvailability"}
// RUN: not --crash %target-swift-frontend -typecheck -experimental-allow-module-with-compiler-errors -cxx-interoperability-mode=default -emit-clang-header-min-access internal -emit-clang-header-path /dev/null %s
struct a {
  @available(*, deprecated, renamed: "c") var b: String
  var c
}

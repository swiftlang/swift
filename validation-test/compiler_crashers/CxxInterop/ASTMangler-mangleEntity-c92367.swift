// {"extraArgs":["-experimental-allow-module-with-compiler-errors","-cxx-interoperability-mode=default","-emit-clang-header-min-access","internal","-emit-clang-header-path","/dev/null"],"kind":"typecheck","original":"0911edd2","signature":"swift::Mangle::ASTMangler::mangleEntity(swift::ValueDecl const*, swift::Mangle::ASTMangler::SymbolKind)","signatureNext":"SILDeclRef::mangle"}
// RUN: not --crash %target-swift-frontend -typecheck -experimental-allow-module-with-compiler-errors -cxx-interoperability-mode=default -emit-clang-header-min-access internal -emit-clang-header-path /dev/null %s
struct a
  struct b {
    c = b<a>(
  }
  struct b<d {  e: [d] =

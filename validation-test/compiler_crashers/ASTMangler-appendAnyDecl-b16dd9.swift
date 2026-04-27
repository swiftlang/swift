// {"extraArgs":["-experimental-allow-module-with-compiler-errors","-emit-module-path","/dev/null"],"kind":"emit-sil","original":"3e01df8a","signature":"swift::Mangle::ASTMangler::appendAnyDecl(swift::ValueDecl const*)","signatureAssert":"Assertion failed: (detail::isPresent(Val) && \"dyn_cast on a non-existent value\"), function dyn_cast","signatureNext":"Mangle::ASTMangler::mangleDeclWithPrefix"}
// RUN: not --crash %target-swift-frontend -emit-sil -experimental-allow-module-with-compiler-errors -emit-module-path /dev/null %s
a {
  struct b {
    @derivative(of: c)<#declaration#>
    d
  }
}

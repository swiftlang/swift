// {"extraArgs":["-experimental-allow-module-with-compiler-errors","-emit-module-path","/dev/null"],"kind":"emit-sil","original":"aef0bffd","signature":"swift::TypeDecl::getName() const","signatureAssert":"Assertion failed: (!bool(ActiveChoice)), function ~ConjunctionStep"}
// The crash depends on the C++ parser's handling of this input.
// UNSUPPORTED: parser_astgen
// RUN: not --crash %target-swift-frontend -emit-sil -experimental-allow-module-with-compiler-errors -emit-module-path /dev/null %s
extension {
  a(b: Bool) -> (Equatable, some FloatingPoint) {&
    if b {
      return (1
      1.0

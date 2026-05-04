// {"extraArgs":["-experimental-allow-module-with-compiler-errors","-emit-module-path","/dev/null"],"kind":"emit-sil","original":"f3a2e711","signature":"swift::TypeDecl::getName() const","signatureAssert":"Assertion failed: (Captures.hasBeenComputed()), function getCaptureInfo"}
// RUN: not --crash %target-swift-frontend -emit-sil -experimental-allow-module-with-compiler-errors -emit-module-path /dev/null %s
@expression macro a<each b>(repeat each b) -> String
extension {
  >(c: @autoclosure () -> String = #a

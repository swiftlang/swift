// {"extraArgs":["-experimental-allow-module-with-compiler-errors"],"kind":"emit-sil","original":"d04192e0","signature":"swift::TypeDecl::getName() const"}
// RUN: not %target-swift-frontend -emit-sil -experimental-allow-module-with-compiler-errors %s
extension {
  @a b, : {

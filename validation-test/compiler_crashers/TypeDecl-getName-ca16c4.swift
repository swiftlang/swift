// {"extraArgs":["-experimental-allow-module-with-compiler-errors","-emit-module-path","/dev/null"],"kind":"emit-sil","original":"53abf90f","signature":"swift::TypeDecl::getName() const","signatureAssert":"Assertion failed: (Index < this->size() && \"Invalid index!\"), function operator[]"}
// RUN: not --crash %target-swift-frontend -emit-sil -experimental-allow-module-with-compiler-errors -emit-module-path /dev/null %s
extension {
  enum a: b, Codable {
    case c(

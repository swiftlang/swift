// {"extraArgs":["-experimental-allow-module-with-compiler-errors"],"kind":"emit-sil","original":"69cabaab","signature":"swift::VarDecl::isSelfParameter() const","signatureAssert":"Assertion failed: (Val && \"isa<> used on a null pointer\"), function doit","signatureNext":"DiagnoseInvalidEscapingCaptures::run"}
// RUN: not --crash %target-swift-frontend -emit-sil -experimental-allow-module-with-compiler-errors %s
func a(b: inout [Int]) {
  func c() {
    b
  }
  struct d {
    var e: () -> Void {
      c
    }
  }
}

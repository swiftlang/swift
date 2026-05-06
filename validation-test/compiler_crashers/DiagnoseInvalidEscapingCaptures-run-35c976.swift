// {"extraArgs":["-experimental-allow-module-with-compiler-errors"],"kind":"emit-sil","original":"69cabaab","signature":"(anonymous namespace)::DiagnoseInvalidEscapingCaptures::run()","signatureNext":"SILPassManager::runPassOnFunction"}
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

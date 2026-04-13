// {"kind":"emit-silgen","original":"23dc0d7a","signature":"swift::Lowering::SILGenFunction::emitCaptures(swift::SILLocation, swift::SILDeclRef, swift::Lowering::CaptureEmission, llvm::SmallVectorImpl<swift::Lowering::ManagedValue>&)","signatureAssert":"Assertion failed: (val->getType().isAddress() && \"no address for captured var!\"), function emitCaptures","signatureNext":"SILGenApply::visitDeclRefExpr"}
// RUN: not --crash %target-swift-frontend -emit-silgen %s
struct a: ~Copyable {
  func b() -> Bool {
  }
}
func c<d>(e: String, f: Int, body: (borrowing a) -> d) {
}
func g(h: [(String, Int)]) {
  h.compactMap {
    e, f in
    c(e: e, f: f) { j in
      func k() -> Bool {
        j.b()
      }
      func i() {
        guard k() else {
        }
      }
      i()
    }
  }
}

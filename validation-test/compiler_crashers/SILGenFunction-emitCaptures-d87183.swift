// {"kind":"emit-silgen","original":"eab5973c","signature":"swift::Lowering::SILGenFunction::emitCaptures(swift::SILLocation, swift::SILDeclRef, swift::Lowering::CaptureEmission, llvm::SmallVectorImpl<swift::Lowering::ManagedValue>&)","signatureAssert":"Assertion failed: (val->getType().isAddress() && \"no address for captured var!\"), function emitCaptures","signatureNext":"Lowering::SILGenFunction::emitClosureValue"}
// RUN: not --crash %target-swift-frontend -emit-silgen %s
struct a: ~Copyable {
  let b: UInt64
}
func c<d>(
  _: borrowing a,
  g: (borrowing a) -> d
) {
  let e = a(b: 0)
  c(e) { f in
    {
      f
    }
  }
}

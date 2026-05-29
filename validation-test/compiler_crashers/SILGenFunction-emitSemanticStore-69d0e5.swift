// {"kind":"emit-silgen","original":"f3a4601c","signature":"swift::Lowering::SILGenFunction::emitSemanticStore(swift::SILLocation, swift::SILValue, swift::SILValue, swift::Lowering::TypeLowering const&, swift::IsInitialization_t)","signatureAssert":"Assertion failed: (!silConv.useLoweredAddresses() || (dest->getType().isAddressOnly(F) == rvalue->getType().isAddress())), function emitSemanticStore","signatureNext":"Lowering::SILGenFunction::emitThrow"}
// RUN: not --crash %target-swift-frontend -emit-silgen %s
protocol a {
  associatedtype b
  associatedtype c: AsyncSequence
  func d(e: b) -> c
}
protocol f {
  associatedtype n: a
}
func g<h, i: f>(
  j: h,
  k: i,
  o: h.b
) async where i.n == h {
  let l = j.d(e: o)
  try! await l.first(where: { m in
    true
  })
}

// {"kind":"emit-silgen","original":"1dd64b16","signature":"swift::Lowering::SILGenFunction::emitOpenExistential(swift::SILLocation, swift::Lowering::ManagedValue, swift::SILType, swift::AccessKind)","signatureAssert":"Assertion failed: (!silConv.useLoweredAddresses() && \"Non-address loweredOpenedType is only allowed under opaque \" \"value mode\"), function emitOpenExistential","signatureNext":"Lowering::SILGenFunction::emitOpenExistentialExprImpl"}
// RUN: not --crash %target-swift-frontend -emit-silgen %s
protocol a<b> {
  associatedtype b
  associatedtype c: d where c.e == Self
  func f(for: b)
}
protocol d {
  associatedtype e: AnyObject
}
func g(
  h: a<String>,
  i: String
) {
  h.f(for: i)
}

// {"kind":"emit-silgen","original":"20252719","signature":"swift::Lowering::Conversion::emit(swift::Lowering::SILGenFunction&, swift::SILLocation, swift::Lowering::ManagedValue, swift::Lowering::SGFContext) const","signatureAssert":"Assertion failed: (value.getType().getObjectType() == getReabstractionInputLoweredType().getObjectType()), function emit","signatureNext":"Lowering::SILGenFunction::emitConvertedRValue"}
// RUN: not --crash %target-swift-frontend -emit-silgen %s
extension Array {
  typealias a = (Element) -> Bool
  func b(c: borrowing a) {
    filter(c)
  }
}

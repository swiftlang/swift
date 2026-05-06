// {"kind":"emit-silgen","original":"dee539d4","signature":"convertFunctionRepresentation(swift::Lowering::SILGenFunction&, swift::SILLocation, swift::Lowering::ManagedValue, swift::CanTypeWrapper<swift::AnyFunctionType>, swift::CanTypeWrapper<swift::AnyFunctionType>)","signatureAssert":"Assertion failed: (value->getOwnershipKind() != OwnershipKind::None), function forOwnedObjectRValue","signatureNext":"RValueEmitter::visitFunctionConversionExpr"}
// RUN: not --crash %target-swift-frontend -emit-silgen %s
func a<b>(
  c: b
) {
  typealias d = @convention(thin) (Int) -> Int
  let f: d
  let e: (Int) -> Int = f
}

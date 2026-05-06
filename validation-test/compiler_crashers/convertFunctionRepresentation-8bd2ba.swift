// {"kind":"emit-silgen","original":"44c57ff3","signature":"convertFunctionRepresentation(swift::Lowering::SILGenFunction&, swift::SILLocation, swift::Lowering::ManagedValue, swift::CanTypeWrapper<swift::AnyFunctionType>, swift::CanTypeWrapper<swift::AnyFunctionType>)","signatureNext":"RValueEmitter::visitFunctionConversionExpr"}
// RUN: not --crash %target-swift-frontend -emit-silgen %s
func a<b, c>(d: @convention(thin) (b) -> c) {
}
a {
}

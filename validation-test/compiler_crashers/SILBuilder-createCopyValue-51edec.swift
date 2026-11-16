// {"extraArgs":["-enable-sil-opaque-values"],"kind":"emit-silgen","signature":"swift::SILBuilder::createCopyValue(swift::SILLocation, swift::SILValue)","signatureAssert":"Assertion failed: (operand->getType().isObject()), function CopyValueInst","signatureNext":"OpaqueValueTypeLowering::emitCopyValue"}
// RUN: not --crash %target-swift-frontend -emit-silgen -enable-sil-opaque-values %s
func a<each b, each c>(d: (repeat each b)) -> (repeat (each c)) {
}

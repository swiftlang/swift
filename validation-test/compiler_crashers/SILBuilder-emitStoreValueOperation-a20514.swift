// {"kind":"emit-silgen","original":"ddf2d3f1","signature":"swift::SILBuilder::emitStoreValueOperation(swift::SILLocation, swift::SILValue, swift::SILValue, swift::StoreOwnershipQualifier)","signatureAssert":"Assertion failed: (!Src->getType().isAddress()), function emitStoreValueOperation"}
// RUN: not --crash %target-swift-frontend -emit-silgen %s
func a<b>(c: b) {
  let d: b
  (d, !) as? b
}

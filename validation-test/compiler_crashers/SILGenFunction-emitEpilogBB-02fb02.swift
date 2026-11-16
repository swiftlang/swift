// {"extraArgs":["-enable-sil-opaque-values"],"kind":"emit-silgen","signature":"swift::Lowering::SILGenFunction::emitEpilogBB(swift::SILLocation)","signatureAssert":"Assertion failed: (idx < size()), function operator[]","signatureNext":"Lowering::SILGenFunction::emitEpilog"}
// RUN: not --crash %target-swift-frontend -emit-silgen -enable-sil-opaque-values %s
func a<each b>(c: repeat each b) -> (Int, (repeat each b), Bool) {
  (2, (repeat each c), true)
}

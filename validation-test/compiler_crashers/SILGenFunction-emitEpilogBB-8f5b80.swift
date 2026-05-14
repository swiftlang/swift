// {"kind":"emit-silgen","original":"528f6a15","signature":"swift::Lowering::SILGenFunction::emitEpilogBB(swift::SILLocation)","signatureAssert":"Assertion failed: (!Cleanups.hasAnyActiveCleanups(getCleanupsDepth(), ReturnDest.getDepth()) && \"emitting epilog in wrong scope\"), function emitEpilogBB","signatureNext":"Lowering::SILGenFunction::emitEpilog"}
// RUN: not --crash %target-swift-frontend -emit-silgen %s
@propertyWrapper
struct a<b: Comparable> {
  var wrappedValue: b
  init!(wrappedValue: b, bounds: ClosedRange<b>) {
  }
}
struct c {
  @a(bounds: 1...1) var d = 0
}

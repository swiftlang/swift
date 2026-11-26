// {"frontendArgs":["-emit-ir","-O","-disable-objc-attr-requires-foundation-module"],"kind":"custom","signature":"swift::irgen::emitMetatypeRef(swift::irgen::IRGenFunction&, swift::CanTypeWrapper<swift::MetatypeType>, swift::irgen::Explosion&)","signatureAssert":"Assertion failed: (protocols.size() == 1 && \"ObjC existential should have one protocol\"), function emitObjCMetatypeRef","signatureNext":"IRGenSILFunction::visitMetatypeInst"}
// RUN: not --crash %target-swift-frontend -emit-ir -O -disable-objc-attr-requires-foundation-module %s
// REQUIRES: objc_interop
@objc protocol P: ~Copyable {
}
func f<T: AnyObject>(_: T.Type) -> AnyObject {
  T.self
}
print(f(P.self))

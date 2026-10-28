// {"kind":"emit-silgen","original":"941ad66b","signature":"swift::Lowering::SILGenFunction::emitSemanticStore(swift::SILLocation, swift::SILValue, swift::SILValue, swift::Lowering::TypeLowering const&, swift::IsInitialization_t)","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast","signatureNext":"Lowering::ManagedValue::forwardInto"}
// RUN: not --crash %target-swift-frontend -emit-silgen %s
@available(SwiftStdlib 5.9, *)
struct a<each b> {
  var c: (repeat each b) {
    (repeat {
      each c
    })
  }
}

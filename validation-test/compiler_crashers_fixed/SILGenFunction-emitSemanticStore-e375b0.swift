// {"kind":"emit-silgen","signature":"swift::Lowering::SILGenFunction::emitSemanticStore(swift::SILLocation, swift::SILValue, swift::SILValue, swift::Lowering::TypeLowering const&, swift::IsInitialization_t)","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast","signatureNext":"Lowering::ManagedValue::assignInto"}
// RUN: %target-swift-frontend -emit-silgen %s -o /dev/null
actor a {
  var b: Any
  init(c: Int) async {
    if c == 0 {
      b = #isolation
    }
  }
}

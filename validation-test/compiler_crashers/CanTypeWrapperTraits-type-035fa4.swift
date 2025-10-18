// {"kind":"emit-silgen","original":"caeb568b","signature":"swift::CanTypeWrapperTraits<swift::SILFunctionType>::type swift::SILType::castTo<swift::SILFunctionType>() const","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast"}
// RUN: not --crash %target-swift-frontend -emit-silgen %s
func a<b, each c>(d
  : repeat Optional<each c>, body
  : (repeat UnsafePointer<each c>) -> b) {
  body(repeat {
      each d
    })
}

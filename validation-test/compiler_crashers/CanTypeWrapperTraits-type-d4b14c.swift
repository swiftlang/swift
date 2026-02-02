// {"kind":"emit-silgen","original":"941ad66b","signature":"swift::CanTypeWrapperTraits<swift::ReferenceStorageType>::type swift::SILType::castTo<swift::ReferenceStorageType>() const","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast"}
// RUN: not --crash %target-swift-frontend -emit-silgen %s
@available(SwiftStdlib 5.9, *)
struct a<each b> {
  var c: (repeat each b) {
    (repeat {
      each c
    })
  }
}

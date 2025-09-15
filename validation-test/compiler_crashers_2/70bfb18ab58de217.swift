// {"kind":"typecheck","signature":"swift::GenericContext::getGenericSignature() const","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a < let b : c
  class c < b : a

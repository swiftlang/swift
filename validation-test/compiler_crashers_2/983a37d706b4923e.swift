// {"kind":"typecheck","signature":"swift::TypeBase::getNominalParent()","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a {
  typealias b<c> = () extension b

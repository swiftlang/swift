// {"kind":"typecheck","original":"1f78965c","signature":"swift::TypeBase::getContextSubstitutionMap()","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast","signatureNext":"LookupConformanceRequest::evaluate"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a {
  associatedtype b: a
}
protocol c: a
  protocol d: c
    struct e<f>: d {
      struct b<g>: d
        typealias b = e.b

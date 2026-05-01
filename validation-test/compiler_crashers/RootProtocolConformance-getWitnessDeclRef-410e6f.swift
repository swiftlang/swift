// {"kind":"typecheck","original":"32fda7e2","signature":"swift::RootProtocolConformance::getWitnessDeclRef(swift::ValueDecl*) const","signatureAssert":"Assertion failed: (!witnessDC->isInnermostContextGeneric()), function getWitnessDeclRef","signatureNext":"ProtocolConformanceRef::getWitnessByName"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a: ExpressibleByStringInterpolation {
  stringInterpolation: b
  struct b: StringInterpolationProtocol {
    init(some Int,  Int)
    func appendLiteral( String)
      appendInterpolation( Int)
    c: a =
      "x=\(7)"

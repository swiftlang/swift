// {"kind":"typecheck","original":"2d5134b1","signature":"swift::RootProtocolConformance::getWitnessDeclRef(swift::ValueDecl*) const","signatureAssert":"Assertion failed: (!witnessDC->isInnermostContextGeneric()), function getWitnessDeclRef","signatureNext":"ProtocolConformance::getWitnessDeclRef"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a<b>: ExpressibleByStringInterpolation {
  init<c>(stringInterpolation: c)
  struct StringInterpolation: StringInterpolationProtocol {
    Int, d: Int
    appendLiteral( String)
    appendInterpolation
    let : a = "\()"

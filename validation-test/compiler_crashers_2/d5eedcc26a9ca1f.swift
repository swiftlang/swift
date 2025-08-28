// {"kind":"typecheck","signature":"swift::TypeBase::getReducedShape()","signatureAssert":"Assertion failed: (!isTypeVariableOrMember()), function getReducedShape"}
// RUN: not --crash %target-swift-frontend -typecheck %s
typealias a <b> = ()
extension a {
c {
  d
}
d

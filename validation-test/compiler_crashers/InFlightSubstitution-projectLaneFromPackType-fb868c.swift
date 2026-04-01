// {"kind":"typecheck","original":"a255cc2e","signature":"swift::InFlightSubstitution::projectLaneFromPackType(swift::Type, unsigned int)","signatureAssert":"Assertion failed: (!empty()), function back","signatureNext":"InFlightSubstitution::substType"}
// RUN: not --crash %target-swift-frontend -typecheck %s
func a<each b>() {
  typealias c<each d> = (repeat (each b))
  func e() -> c<>
  e
}

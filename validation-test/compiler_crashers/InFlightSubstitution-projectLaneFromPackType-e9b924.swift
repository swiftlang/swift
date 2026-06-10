// {"kind":"typecheck","original":"23cce1db","signature":"swift::InFlightSubstitution::projectLaneFromPackType(swift::Type, unsigned int)","signatureAssert":"Assertion failed: (substEltType->is<PackExpansionType>() && \"substituted shape mismatch: expected an expansion component\"), function projectLaneFromPackType","signatureNext":"InFlightSubstitution::substType"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a<each b> {
  typealias c<each d> = (repeat (each b, each d))
  typealias e<d> = c<d>
}

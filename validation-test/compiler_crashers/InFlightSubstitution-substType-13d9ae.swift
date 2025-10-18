// {"kind":"typecheck","original":"369523c9","signature":"swift::InFlightSubstitution::substType(swift::SubstitutableType*, unsigned int)","signatureAssert":"Assertion failed: (Ptr && \"Cannot dereference a null Type!\"), function operator->"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a {
  typealias b<c, d> = a
  extension b<Float, Int> {
    extension b {
    }
  }
}

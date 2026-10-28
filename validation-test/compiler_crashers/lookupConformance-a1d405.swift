// {"kind":"typecheck","original":"4df2c2e8","signature":"swift::lookupConformance(swift::Type, swift::ProtocolDecl*, bool)","signatureAssert":"Assertion failed: (Ptr && \"Cannot dereference a null Type!\"), function operator->","signatureNext":"superclassConformsTo"}
// RUN: not --crash %target-swift-frontend -typecheck %s
class a {
  class b: a<>, Codable {
  }
}

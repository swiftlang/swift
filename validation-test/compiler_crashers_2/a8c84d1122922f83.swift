// {"kind":"typecheck","original":"bee4b7dd","signature":"swift::constraints::ContextualFailure::tryProtocolConformanceFixIt() const","signatureAssert":"Assertion failed: (!missingProtoTypeStrings.empty() && \"type already conforms to all the protocols?\"), function tryProtocolConformanceFixIt"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a<b, c> {
  associatedtype b
  associatedtype c
}
extension Int: a {
  struct d<e>: a {
    typealias b = e
  }
  func f() -> a<a, a> {
    d<Int>(<#expression#>)
  }
}

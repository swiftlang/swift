// {"kind":"typecheck","original":"b9eafd0d","signature":"swift::constraints::ContextualFailure::tryProtocolConformanceFixIt() const","signatureAssert":"Assertion failed: (!missingProtoTypeStrings.empty() && \"type already conforms to all the protocols?\"), function tryProtocolConformanceFixIt","signatureNext":"ContextualFailure::diagnoseAsError"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a<b> {
  associatedtype b
}
struct c: a {
  var d: a<Int> = {
    c(<#expression#>)
  }
}

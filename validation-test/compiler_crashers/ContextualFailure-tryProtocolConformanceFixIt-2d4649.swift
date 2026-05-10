// {"extraArgs":["-language-mode","6"],"kind":"typecheck","original":"e6383766","signature":"swift::constraints::ContextualFailure::tryProtocolConformanceFixIt() const","signatureAssert":"Assertion failed: (!missingProtoTypeStrings.empty() && \"type already conforms to all the protocols?\"), function tryProtocolConformanceFixIt","signatureNext":"ContextualFailure::tryFixIts"}
// RUN: not --crash %target-swift-frontend -typecheck -language-mode 6 %s
class a {
  b{ self= \c ?? self
  }

// {"kind":"typecheck","original":"fba90dae","signature":"swift::SubstitutionMap::lookupConformance(swift::CanType, swift::ProtocolDecl*) const","signatureAssert":"Assertion failed: (result && \"Subject type must be exactly equal to left-hand side of a\" \"conformance requirement in protocol requirement signature\"), function getAssociatedConformance","signatureNext":"LookUpConformanceInSubstitutionMap"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a {
  associatedtype b
  func c < d where Self : e, b == <#type#>>()
}
protocol e
: a {
  struct f : a {
c
  }
}

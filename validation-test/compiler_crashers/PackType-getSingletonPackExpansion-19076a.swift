// {"kind":"typecheck","original":"4f0b1241","signature":"swift::PackType::getSingletonPackExpansion(swift::Type)","signatureAssert":"Assertion failed: (rootParameterPacks.size() >= 1), function getSingletonPackExpansion","signatureNext":"BuildForwardingSubstitutions"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a {
  associatedtype b: c
  struct c<each d: a> {
    var d: <#type#>
  }
}

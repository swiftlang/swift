// {"kind":"typecheck","original":"0ff257dd","signature":"swift::ProtocolConformanceRef::getTypeWitness(swift::AssociatedTypeDecl*, swift::SubstOptions) const","signatureAssert":"Assertion failed: (concrete->getProtocol() == assocType->getProtocol()), function getTypeWitness","signatureNext":"TypeSubstituter::transformDependentMemberType"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a {
  associatedtype b
}
protocol c: a {
  func
    d<e>() -> f<b, e>
  struct f<g: Hashable, h>: c {
    typealias b = g
    func d()
  }
}

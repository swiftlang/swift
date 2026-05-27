// {"kind":"typecheck","original":"01f5c2a2","signature":"recordTypeWitness(swift::NormalProtocolConformance*, swift::AssociatedTypeDecl*, swift::Type, swift::TypeDecl*)","signatureAssert":"Assertion failed: (conformance->getTypeWitnessUncached(assocType) .getWitnessType() ->isEqual(type) && \"Conflicting type witness deductions\"), function recordTypeWitness","signatureNext":"ResolveTypeWitnessesRequest::evaluate"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a: AsyncSequence {
  associatedtype Failure
  struct b<Failure: Error>: a {
  }
}

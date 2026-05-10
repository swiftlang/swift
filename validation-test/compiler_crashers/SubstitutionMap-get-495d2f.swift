// {"kind":"typecheck","original":"6cc9045d","signature":"swift::SubstitutionMap::get(swift::GenericSignature, swift::InFlightSubstitution&)","signatureAssert":"Assertion failed: ((replacement->hasError() || gp->isParameterPack() == replacement->is<PackType>()) && \"replacement for pack parameter must be a pack type\"), function get","signatureNext":"SubstitutionMap::getOverrideSubstitutions"}
// RUN: not --crash %target-swift-frontend -typecheck %s
class a {
  func
    b<each c>()
  class d: a {
    func
      b<c>()
  }
}

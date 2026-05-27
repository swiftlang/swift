// {"kind":"typecheck","original":"d77b030b","signature":"swift::SubstitutionMap::get(swift::GenericSignature, swift::InFlightSubstitution&)","signatureAssert":"Assertion failed: ((replacement->hasError() || gp->isParameterPack() == replacement->is<PackType>()) && \"replacement for pack parameter must be a pack type\"), function get","signatureNext":"synthesizeMainBody"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@main actor a {
  static  main<each b>(repeat each b)

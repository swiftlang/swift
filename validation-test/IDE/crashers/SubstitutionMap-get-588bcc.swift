// {"kind":"complete","original":"6eafdc64","signature":"swift::SubstitutionMap::get(swift::GenericSignature, swift::InFlightSubstitution&)","signatureAssert":"Assertion failed: ((replacement->hasError() || gp->isParameterPack() == replacement->is<PackType>()) && \"replacement for pack parameter must be a pack type\"), function get","signatureNext":"TypeBase::getMemberSubstitutionMap"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
enum a<b> {
  c { var current = self switch current { case .
  #^^# }
  }
  enum d<each e> {
    case cancelled
  }
}

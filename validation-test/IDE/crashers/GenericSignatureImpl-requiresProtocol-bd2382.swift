// {"kind":"complete","original":"2bea4203","signature":"swift::GenericSignatureImpl::requiresProtocol(swift::Type, swift::ProtocolDecl*) const","signatureNext":"SubstitutionMap::lookupConformance"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
protocol a {
  associatedtype b
  protocol c<b>: a where d: c<Never> {
    var e: #^^#
  }
}

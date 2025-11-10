// {"kind":"complete","signature":"swift::GenericSignatureImpl::requiresProtocol(swift::Type, swift::ProtocolDecl*) const"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
protocol a {
  associatedtype b
}
protocol c : a where c== d
  extension c {
    typealias c = b
    #^^#

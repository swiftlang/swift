// {"kind":"complete","signature":"swift::GenericSignatureImpl::getSuperclassBound(swift::Type) const"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
protocol a {
  associatedtype b: Error
}
extension a {c
  #^^#
  class Error

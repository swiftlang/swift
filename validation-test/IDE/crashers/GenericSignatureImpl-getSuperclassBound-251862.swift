// {"kind":"complete","original":"89b4bdea","signature":"swift::GenericSignatureImpl::getSuperclassBound(swift::Type) const"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
protocol a {
  a  typealias a =
    #^^#
  convenience
    #b(a)
}

// {"kind":"complete","original":"d87fb92a","signature":"swift::GenericSignatureImpl::areReducedTypeParametersEqual(swift::Type, swift::Type) const","signatureNext":"TypeResolution::areSameType"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
@attached(member) macro a()
protocol b {
  associatedtype c
  @a<c> associatedtype c: #^^#
}

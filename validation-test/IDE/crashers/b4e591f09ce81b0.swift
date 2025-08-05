// {"kind":"complete","original":"0b319c77","signature":"swift::ExtensionDecl::getExtendedNominal() const","signatureAssert":"Extension must have already been bound"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
extension a[ {
  #^^#
}

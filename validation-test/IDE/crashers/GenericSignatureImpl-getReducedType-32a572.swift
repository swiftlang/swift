// {"kind":"complete","original":"4412067b","signature":"swift::GenericSignatureImpl::getReducedType(swift::Type) const","signatureNext":"TypeBase::getReducedType","useSourceOrderCompletion":true}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-order-completion -source-filename %s
@freestanding( declaration arbitrary) macro a protocol b {
#^^#
}
enum c: d {
#^f^#}
extension Int: e }
#a(~)

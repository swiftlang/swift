// {"kind":"complete","original":"6ebea468","signature":"swift::GenericSignatureImpl::getSuperclassBound(swift::Type) const","signatureNext":"TypeResolution::resolveTypeInContext"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
class a {
  protocol b
    protocol c {
      associatedtype f
    }
    protocol d: a, c where f: b
      class e: d {
        #^^#

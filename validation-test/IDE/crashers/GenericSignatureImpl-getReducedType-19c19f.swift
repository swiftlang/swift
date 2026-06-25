// {"kind":"complete","original":"2f484286","signature":"swift::GenericSignatureImpl::getReducedType(swift::Type) const","signatureNext":"InferredGenericSignatureRequest::evaluate"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
protocol a: b
  protocol c {
    associatedtype d: a
    var e {
      #^^#
    }
  }
  struct b<f: c

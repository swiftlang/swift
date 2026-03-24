// {"kind":"complete","original":"2fed565c","signature":"swift::GenericSignature::verify(llvm::ArrayRef<swift::Requirement>) const","signatureNext":"InferredGenericSignatureRequest::evaluate"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
protocol a {
  b
  #b {
    b
  }
}
struct c<d: a>
    a    #^^#

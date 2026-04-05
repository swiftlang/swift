// {"kind":"complete","original":"59bce891","signature":"swift::Mangle::ASTMangler::gatherGenericSignatureParts(swift::GenericSignature, swift::GenericSignature, swift::Mangle::ASTMangler::BaseEntitySignature&, swift::Mangle::ASTMangler::GenericSignatureParts&)","signatureAssert":"Assertion failed: (!mangledDepth || *mangledDepth <= depth), function setDepth","signatureNext":"Mangle::ASTMangler::appendDeclType"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
struct a<b> {
  enum c<d> {
    extension c {
      extension e {
        struct h<f> {
          >
          let g = #^^#
        }
      }
    }
  }
}

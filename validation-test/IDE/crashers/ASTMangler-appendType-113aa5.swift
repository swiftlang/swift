// {"kind":"complete","original":"3a7f38eb","signature":"swift::Mangle::ASTMangler::appendType(swift::Type, swift::GenericSignature, swift::ValueDecl const*)","signatureAssert":"Assertion failed: (paramTy->isCanonical() && \"cannot mangle non-canonical generic parameter\"), function appendType"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
class a<b> {
  func
    c<let d: b>()
  {
    #^^#
  }
}

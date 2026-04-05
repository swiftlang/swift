// {"kind":"complete","original":"8c74319b","signature":"swift::Mangle::ASTMangler::appendType(swift::Type, swift::GenericSignature, swift::ValueDecl const*)","signatureAssert":"Assertion failed: (paramTy->isCanonical() && \"cannot mangle non-canonical generic parameter\"), function appendType","signatureNext":"Mangle::ASTMangler::appendBoundGenericArgs"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
func b() {
  #^^#
}
struct c<d> {
  =<
  let a: UnsafePointer<d>
}

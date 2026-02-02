// {"kind":"complete","original":"dbc89b63","signature":"swift::Mangle::ASTMangler::appendDeclName(swift::ValueDecl const*, swift::DeclBaseName, bool)","signatureAssert":"Assertion failed: (!getABIDecl(decl) && \"caller should make sure we get ABI decls\"), function appendDeclName"}
// RUN: %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
@abi(func a()) struct b {
  @#^^#< #declaration
  #  >
}

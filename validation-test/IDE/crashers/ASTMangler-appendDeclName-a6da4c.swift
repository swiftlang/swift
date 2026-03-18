// {"kind":"complete","original":"9fd5bc52","signature":"swift::Mangle::ASTMangler::appendDeclName(swift::ValueDecl const*, swift::DeclBaseName, bool)","signatureAssert":"Assertion failed: (AllowNamelessEntities && \"attempt to mangle unnamed decl\"), function appendDeclName","signatureNext":"Mangle::ASTMangler::appendAccessorEntity"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
var : {
  struct a {    b -> Self { #^^#

// {"kind":"complete","original":"070481b6","signature":"swift::Mangle::ASTMangler::appendEntity(swift::ValueDecl const*)","signatureAssert":"Assertion failed: (!isa<ConstructorDecl>(decl)), function appendEntity"}
// RUN: %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
@abi( init () ) func a
#^^#()

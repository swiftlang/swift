// {"kind":"complete","original":"408c84fb","signature":"swift::Mangle::ASTMangler::appendDeclName(swift::ValueDecl const*, swift::DeclBaseName, bool)","signatureAssert":"Assertion failed: (AllowNamelessEntities && \"attempt to mangle unnamed decl\"), function appendDeclName","signatureNext":"Mangle::ASTMangler::appendEntity"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
struct a { _ = b { protocol c func d -> c {
#^^#}
} {

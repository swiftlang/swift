// {"kind":"complete","original":"2201e4ed","signature":"swift::Mangle::ASTMangler::mangleAnyDecl(swift::ValueDecl const*, bool)","signatureNext":"Mangle::ASTMangler::mangleDeclWithPrefix"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
enum a<b{ protocol c{ class d{ class f<b let e=f #^^#

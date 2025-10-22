// {"kind":"complete","original":"2201e4ed","signature":"swift::Mangle::ASTMangler::mangleDeclAsUSR(swift::ValueDecl const*, llvm::StringRef)"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
enum a<b{ protocol c{ class d{ class f<b let e=f #^^#

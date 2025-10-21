// {"kind":"complete","original":"d09c9cf7","signature":"swift::Mangle::ASTMangler::mangleTypeAsUSR(swift::Type)"}
// RUN: %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
try #^^# ?? (try? a.() as? b)

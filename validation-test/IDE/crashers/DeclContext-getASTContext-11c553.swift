// {"kind":"complete","original":"26ecb72e","signature":"swift::DeclContext::getASTContext() const","signatureNext":"FreestandingMacroExpansion::getDiscriminator"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
@a(
#b #^^# var c

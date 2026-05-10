// {"kind":"complete","original":"d856a15d","signature":"swift::Mangle::Mangler::verify(llvm::StringRef, swift::Mangle::ManglingFlavor)","signatureNext":"Mangle::ASTMangler::mangleTypeAsUSR"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
struct a<let b> {
  let <#pattern#>: a<0xFFFF_FFFF> =
    #^^#
}

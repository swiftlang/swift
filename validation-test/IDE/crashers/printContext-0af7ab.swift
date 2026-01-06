// {"kind":"complete","original":"33e6423a","signature":"swift::printContext(llvm::raw_ostream&, swift::DeclContext*)"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
extension {
  subscript -> {
    @
    #^^#

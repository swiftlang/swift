// {"kind":"complete","original":"7f9ad574","signature":"swift::printContext(llvm::raw_ostream&, swift::DeclContext*)","signatureNext":"ValueDecl::dumpRef"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
extension {
  struct a {
b { var c(#^^#= c

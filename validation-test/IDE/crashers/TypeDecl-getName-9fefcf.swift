// {"kind":"complete","original":"2ab8a74a","signature":"swift::TypeDecl::getName() const"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
extension {
  subscript -> a {
    @
    #^^#

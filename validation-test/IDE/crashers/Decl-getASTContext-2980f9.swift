// {"kind":"complete","original":"33ecb328","signature":"swift::Decl::getASTContext() const"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
protocol a {b
  protocol c
    @attached(extension conformances: c ) macro d =
    #^^#
    #if
    #e(b

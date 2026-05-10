// {"kind":"complete","original":"b9879b84","signature":"swift::DeclContext::getASTContext() const","signatureNext":"VarDecl::getPropertyWrapperAuxiliaryVariables"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
@a({
  $0
  @#^^#
} var b

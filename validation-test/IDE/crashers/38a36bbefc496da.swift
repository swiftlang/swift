// {"kind":"complete","original":"61e9155a","signature":"swift::ide::CodeCompletionResultBuilder::takeResult()"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
extension ()
where
  #^^# == <#type#>
{
}

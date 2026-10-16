// {"kind":"complete","original":"94a984af","signature":"swift::ide::CodeCompletionResultBuilder::takeResult()","signatureNext":"CodeCompletionResultBuilder::finishResult"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
enum a<each b> {
  case (repeat each b)
}
a<
>#^^#

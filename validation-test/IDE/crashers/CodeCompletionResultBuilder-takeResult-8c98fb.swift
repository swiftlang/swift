// {"kind":"complete","original":"94a984af","signature":"swift::ide::CodeCompletionResultBuilder::takeResult()","signatureAssert":"Abort: function verifyUSRToDeclReconstruction: Reconstructed declaration shouldn't be null","signatureNext":"CompletionLookup::addEnumElementRef"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
enum a<each b> {
  case (repeat each b)
}
a<
>#^^#
